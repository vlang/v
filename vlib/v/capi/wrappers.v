module capi

import strings

pub struct ExportInfo {
pub:
	c_name    string
	signature string
	doc       string
}

// generate_runtime returns the V source of the small runtime used by the
// generated wrappers. It lives in a .c.v file so C interop stays explicit.
pub fn generate_runtime(m ApiModel) string {
	return generate_runtime_for(m, m.module_name, m.prefix)
}

// generate_runtime_for emits the shared runtime into an explicit module with
// an explicit symbol prefix, used by combined multi-module builds.
pub fn generate_runtime_for(m ApiModel, module_name string, prefix string) string {
	return 'module ${module_name}

__global (
	capi_rt_error_buf []u8
)

fn capi_rt_cstr(p &char) string {
	if p == unsafe { nil } {
		return \'\'
	}
	return unsafe { cstring_to_vstring(p) }
}

fn capi_rt_strdup(s string) voidptr {
	buf := unsafe { C.malloc(s.len + 1) }
	if buf == unsafe { nil } {
		return unsafe { nil }
	}
	if s.len > 0 {
		unsafe { C.memcpy(buf, s.str, s.len) }
	}
	unsafe { (&u8(buf))[s.len] = 0 }
	return buf
}

fn capi_rt_set_error(msg string) {
	capi_rt_error_buf = msg.bytes()
	capi_rt_error_buf << u8(0)
}

@[export: \'${prefix}_last_error\']
fn capi_rt_last_error() &char {
	if capi_rt_error_buf.len == 0 {
		capi_rt_error_buf = \'no error\'.bytes()
		capi_rt_error_buf << u8(0)
	}
	return &char(capi_rt_error_buf.data)
}

@[export: \'${prefix}_free\']
fn capi_rt_free(p voidptr) {
	if p != unsafe { nil } {
		unsafe { C.free(p) }
	}
}

@[export: \'${prefix}_init\']
fn capi_rt_init() int {
	return 0
}
'
}

// generate_wrappers returns the V source with one exported C-ABI wrapper per
// supported public declaration.
pub fn generate_wrappers(m ApiModel, mut warnings []string) (string, []ExportInfo) {
	mut b := strings.Builder{}
	mut infos := []ExportInfo{}
	b.writeln('module ${if qualified_model(m) { 'main' } else { m.module_name }}')
	b.writeln('')
	for fi, f in m.funcs {
		if wrapper_for_func(m, fi, f, mut b, mut warnings, mut infos) {
			b.writeln('')
		}
	}
	for c in m.consts {
		if wrapper_for_const(m, c, mut b, mut warnings, mut infos) {
			b.writeln('')
		}
	}
	return b.str(), infos
}

fn qualified_model(m ApiModel) bool {
	return m.funcs.len > 0 && m.funcs[0].qualifier != ''
}

// qname prefixes a symbol with its import alias when wrappers are generated
// outside the declaring module.
fn qname(f ApiFunc, name string) string {
	return if f.qualifier == '' { name } else { '${f.qualifier}.${name}' }
}

fn wrapper_for_func(m ApiModel, idx int, f ApiFunc, mut b strings.Builder, mut warnings []string, mut infos []ExportInfo) bool {
	mut inner := unwrap(f.return_type)
	has_result := is_result(f.return_type) || is_option(f.return_type)
	if has_result {
		inner = unwrap(f.return_type)
		if is_slice(inner) {
			warnings << 'skipped ${f.name}: result slices are not supported yet'
			return false
		}
	} else if is_slice(f.return_type) {
		warnings << 'skipped ${f.name}: plain slice returns are not supported yet'
		return false
	}
	ret_v := if f.return_type == '' {
		''
	} else {
		v_wrapper_type(f.return_type, m) or {
			warnings << 'skipped ${f.name}: unsupported return type ${f.return_type}'
			return false
		}
	}
	mut decl_params := []string{}
	mut c_params := []string{}
	mut call_args := []string{}
	mut setup := []string{}
	if f.is_method {
		base := unqualified(strip_ref(f.receiver))
		if !m.is_struct(base) || !f.receiver.starts_with('&') {
			warnings << 'skipped ${f.name}: only pointer receivers on public structs are supported'
			return false
		}
		decl_params << 'self voidptr'
		c_params << 'void *self'
		setup << '\trecv := unsafe { &${qname(f, base)}(self) }'
	}
	for i, p in f.params {
		if p.is_receiver {
			continue
		}
		ptype := v_wrapper_type(p.type_name, m) or {
			warnings << 'skipped ${f.name}: unsupported parameter type ${p.type_name}'
			return false
		}
		name := 'p${i}'
		if is_slice(p.type_name) {
			elem := strip_ref(slice_elem(p.type_name))
			decl_params << '${name} &${c_prim(elem)}'
			decl_params << '${name}_len int'
			c_params << 'const ${c_elem_type(elem)} *${name}'
			c_params << 'int ${name}_len'
			mut arr := '\tmut ${name}_v := []${c_prim(elem)}{len: ${name}_len}'
			setup << arr
			setup << '\tif ${name}_len > 0 { unsafe { C.memcpy(${name}_v.data, ${name}, ${name}_len * sizeof(${c_prim(elem)})) } }'
			call_args << '${name}_v'
			continue
		}
		decl_params << '${name} ${ptype}'
		cparam_type := c_decl_type(p.type_name, m) or {
			warnings << 'skipped ${f.name}: unsupported parameter type ${p.type_name}'
			return false
		}
		if p.type_name == 'string' {
			c_params << 'const char *${name}'
			setup << '\t${name}_v := capi_rt_cstr(${name})'
			call_args << '${name}_v'
		} else if p.type_name == 'bool' {
			call_args << '${name} != 0'
		} else if m.is_enum(unqualified(strip_ref(p.type_name))) {
			etype := unqualified(strip_ref(p.type_name))
			call_args << 'unsafe { ${qname(f, etype)}(${name}) }'
		} else if p.type_name.starts_with('&') && m.is_struct(unqualified(strip_ref(p.type_name))) {
			stype := unqualified(strip_ref(p.type_name))
			if p.is_mut {
				// `mut c T` is encoded as `&T` with the mut flag; pass a mutable
				// copy of the pointed-to value rather than the pointer itself.
				setup << '\tmut ${name}_v := unsafe { *(&${qname(f, stype)}(${name})) }'
				call_args << 'mut ${name}_v'
			} else {
				setup << '\t${name}_v := unsafe { &${qname(f, stype)}(${name}) }'
				call_args << '${name}_v'
			}
		} else {
			call_args << name
		}
		if p.type_name != 'string' {
			c_params << '${cparam_type} ${name}'
		}
	}
	mut ret_c := 'void'
	if has_result {
		decl_params << 'out &${ret_v}'
		mut out_c := c_decl_type(inner, m) or {
			warnings << 'skipped ${f.name}: unsupported result type ${inner}'
			return false
		}
		out_c = if inner == 'string' { 'char *' } else { out_c }
		c_params << '${out_c} *out'
		ret_c = 'int'
	} else if f.return_type != '' {
		ret_c = c_decl_type(f.return_type, m) or { return false }
	}
	mut ret_spelling := if ret_v == '' { '' } else { ' ${ret_v}' }
	if has_result {
		ret_spelling = ' int'
	}
	b.writeln("@[export: '${f.c_name}']")
	b.writeln('fn capi_export_${idx}(${decl_params.join(', ')})${ret_spelling} {')
	for line in setup {
		b.writeln(line)
	}
	call := if f.is_method {
		'recv.${f.name.all_after_last('.')}(${call_args.join(', ')})'
	} else {
		'${qname(f, f.name)}(${call_args.join(', ')})'
	}
	if has_result {
		if is_result(f.return_type) {
			b.writeln('\tv := ${call} or { capi_rt_set_error(err.msg()); return -1 }')
		} else {
			b.writeln('\tv := ${call} or { return 1 }')
		}
		b.writeln('\tunsafe { out[0] = ${out_value(inner, m, 'v')} }')
		b.writeln('\treturn 0')
	} else if f.return_type == '' {
		b.writeln('\t${call}')
	} else {
		b.writeln('\treturn ${out_value(f.return_type, m, call)}')
	}
	b.writeln('}')
	infos << ExportInfo{
		c_name:    f.c_name
		signature: '${ret_c} ${f.c_name}(${c_params.join(', ')})'
		doc:       f.doc
	}
	return true
}

fn out_value(t string, m ApiModel, expr string) string {
	if t == 'string' {
		return '&char(capi_rt_strdup(${expr}))'
	}
	if t == 'bool' {
		return 'if ${expr} { 1 } else { 0 }'
	}
	if m.is_enum(unqualified(strip_ref(t))) {
		return 'int(${expr})'
	}
	if t.starts_with('&') && m.is_struct(unqualified(strip_ref(t))) {
		return 'voidptr(${expr})'
	}
	return expr
}

fn wrapper_for_const(m ApiModel, c ApiConst, mut b strings.Builder, mut warnings []string, mut infos []ExportInfo) bool {
	if c.value == '' {
		warnings << 'skipped const ${c.name}: value could not be read'
		return false
	}
	cname := '${m.prefix}_get_${sanitize(c.name)}'
	t := if c.value.starts_with("'") || c.value.starts_with('"') { 'string' } else { 'int' }
	rt := v_wrapper_type(t, m) or { return false }
	b.writeln("@[export: '${cname}']")
	b.writeln('fn capi_${cname}() ${rt} {')
	if t == 'string' {
		b.writeln('\treturn &char(capi_rt_strdup(${c.value}))')
	} else {
		b.writeln('\treturn ${c.value}')
	}
	b.writeln('}')
	infos << ExportInfo{
		c_name:    cname
		signature: '${if t == 'string' { 'char *' } else { 'int' }} ${cname}(void)'
		doc:       c.doc
	}
	return true
}
