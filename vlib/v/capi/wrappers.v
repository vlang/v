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

fn capi_rt_slice_copy(data voidptr, len int, elem_size u32) voidptr {
	if len <= 0 || data == unsafe { nil } {
		return unsafe { nil }
	}
	buf := unsafe { C.malloc(len * elem_size) }
	if buf == unsafe { nil } {
		return unsafe { nil }
	}
	unsafe { C.memcpy(buf, data, len * elem_size) }
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
	mut used := map[string]bool{}
	b.writeln('module ${if qualified_model(m) { 'main' } else { m.module_name }}')
	b.writeln('')
	for fi, f0 in m.funcs {
		mut f := f0
		f.c_name = unique_export_name(used, f.c_name)
		if wrapper_for_func(m, fi, f, mut b, mut warnings, mut infos) {
			used[infos.last().c_name] = true
			b.writeln('')
		}
	}
	for ci, c0 in m.consts {
		mut c := c0
		c.c_name = unique_export_name(used, c.c_name)
		if wrapper_for_const(m, ci, c, mut b, mut warnings, mut infos) {
			used[infos.last().c_name] = true
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

fn is_local_enum(m ApiModel, type_name string, qualifier string) bool {
	t := type_by_name_for(m, unqualified(strip_ref(type_name)), qualifier) or { return false }
	return t.kind == 'enum' && t.qualifier == qualifier
}

fn unique_export_name(used map[string]bool, name string) string {
	if !used[name] {
		return name
	}
	mut n := 2
	for {
		candidate := '${name}_${n}'
		if !used[candidate] {
			return candidate
		}
		n++
	}
}

fn wrapper_for_func(m ApiModel, idx int, f ApiFunc, mut b strings.Builder, mut warnings []string, mut infos []ExportInfo) bool {
	mut inner := unwrap(f.return_type)
	has_result := is_result(f.return_type) || is_option(f.return_type)
	err_code := if f.error_code != 0 { f.error_code.str() } else { '-1' }
	returns_slice := is_slice(inner)
	if returns_slice && !is_supported_slice_elem(slice_elem(inner)) {
		warnings << 'skipped ${f.name}: unsupported slice element ${slice_elem(inner)}'
		return false
	}
	ret_v := if f.return_type == '' || returns_slice {
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
		t := type_by_name_for(m, base, f.qualifier) or {
			warnings << 'skipped ${f.name}: receiver type is not a public struct in its module'
			return false
		}
		if t.kind != 'struct' || t.qualifier != f.qualifier {
			warnings << 'skipped ${f.name}: receiver type belongs to another module'
			return false
		}
		decl_params << 'self voidptr'
		c_params << 'void *self'
		if f.receiver.starts_with('&') {
			setup << '\trecv := unsafe { &${type_qname(m, base, f.qualifier)}(self) }'
		} else {
			// A value receiver needs a value copy of the pointed-to object.
			setup << '\tmut recv := unsafe { *(&${type_qname(m, base, f.qualifier)}(self)) }'
		}
	}
	for i, p in f.params {
		if p.is_receiver {
			continue
		}
		pt := type_by_name_for(m, unqualified(strip_ref(p.type_name)), f.qualifier) or { ApiType{} }
		if pt.name != '' && pt.qualifier != f.qualifier {
			warnings << 'skipped ${f.name}: parameter type belongs to another module'
			return false
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
		v_type := if m.is_struct(unqualified(ptype)) && !ptype.starts_with('&') {
			type_qname(m, ptype, f.qualifier)
		} else {
			ptype
		}
		decl_params << '${name} ${v_type}'
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
		} else if is_local_enum(m, p.type_name, f.qualifier) {
			etype := unqualified(strip_ref(p.type_name))
			call_args << 'unsafe { ${type_qname(m, etype, f.qualifier)}(${name}) }'
		} else if p.type_name.starts_with('&') && m.is_struct(unqualified(strip_ref(p.type_name))) {
			stype := unqualified(strip_ref(p.type_name))
			if p.is_mut {
				// `mut c T` is encoded as `&T` with the mut flag; pass a mutable
				// copy of the pointed-to value rather than the pointer itself.
				setup << '\tmut ${name}_v := unsafe { *(&${type_qname(m, stype, f.qualifier)}(${name})) }'
				call_args << 'mut ${name}_v'
			} else {
				setup << '\t${name}_v := unsafe { &${type_qname(m, stype, f.qualifier)}(${name}) }'
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
	mut ret_spelling := if ret_v == '' {
		''
	} else if m.is_struct(unqualified(ret_v)) {
		' ${type_qname(m, ret_v, f.qualifier)}'
	} else {
		' ${ret_v}'
	}
	if returns_slice {
		elem := c_prim(slice_elem(inner))
		decl_params << 'out &&${elem}'
		decl_params << 'out_len &int'
		c_params << '${c_elem_type(slice_elem(inner))} **out'
		c_params << 'int *out_len'
		ret_c = 'int'
		ret_spelling = ' int'
	} else if has_result {
		out_v := if m.is_struct(unqualified(ret_v)) {
			type_qname(m, ret_v, f.qualifier)
		} else {
			ret_v
		}
		decl_params << 'out &${out_v}'
		mut out_c := c_decl_type(inner, m) or {
			warnings << 'skipped ${f.name}: unsupported result type ${inner}'
			return false
		}
		out_c = if inner == 'string' { 'char *' } else { out_c }
		c_params << '${out_c} *out'
		ret_c = 'int'
		ret_spelling = ' int'
	} else if f.return_type != '' {
		ret_c = c_decl_type(f.return_type, m) or { return false }
	}
	b.writeln("@[export: '${f.c_name}']")
	b.writeln('fn capi_export_${idx}(${decl_params.join(', ')})${ret_spelling} {')
	for line in setup {
		b.writeln(line)
	}
	mut call := if f.is_method {
		'recv.${f.name.all_after_last('.')}(${call_args.join(', ')})'
	} else {
		'${qname(f, f.name)}(${call_args.join(', ')})'
	}
	if f.is_unsafe {
		call = 'unsafe { ${call} }'
	}
	if returns_slice {
		if has_result {
			if is_result(f.return_type) {
				b.writeln('\tv := ${call} or { capi_rt_set_error(err.msg()); return ${err_code} }')
			} else {
				b.writeln('\tv := ${call} or { return 1 }')
			}
		} else {
			b.writeln('\tv := ${call}')
		}
		elem := c_prim(slice_elem(inner))
		b.writeln('\tunsafe { out[0] = &${elem}(capi_rt_slice_copy(voidptr(v.data), v.len, sizeof(${elem}))) }')
		b.writeln('\tunsafe { out_len[0] = v.len }')
		b.writeln('\treturn 0')
	} else if has_result {
		if is_result(f.return_type) {
			b.writeln('\tv := ${call} or { capi_rt_set_error(err.msg()); return ${err_code} }')
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

fn is_literal_const(value string) bool {
	if value.len >= 2 && ((value.starts_with("'") && value.ends_with("'"))
		|| (value.starts_with('"') && value.ends_with('"'))) {
		return true
	}
	if value == 'true' || value == 'false' {
		return true
	}
	mut i := 0
	if value.starts_with('-') || value.starts_with('+') {
		i = 1
	}
	if value.len > i + 2 && value[i] == `0` && (value[i + 1] == `x` || value[i + 1] == `X`) {
		i += 2
	}
	if i >= value.len {
		return false
	}
	for c in value[i..] {
		if !((c >= `0` && c <= `9`) || (c >= `a` && c <= `f`) || (c >= `A` && c <= `F`) || c == `_`) {
			return false
		}
	}
	return true
}

fn wrapper_for_const(m ApiModel, idx int, c ApiConst, mut b strings.Builder, mut warnings []string, mut infos []ExportInfo) bool {
	if c.value == '' {
		warnings << 'skipped const ${c.name}: value could not be read'
		return false
	}
	value := c.value.all_before('//').trim_space()
	if value == '' || !is_literal_const(value) {
		warnings << 'skipped const ${c.name}: only literal constants are supported'
		return false
	}
	cname := if c.c_name != '' { c.c_name } else { '${m.prefix}_get_${sanitize(c.name)}' }
	mut t := if value.starts_with("'") || value.starts_with('"') {
		'string'
	} else if value == 'true' || value == 'false' {
		'bool'
	} else {
		'int'
	}
	// Large hex literals (e.g. 0xB5026F5AA96619E9) are u64 in V, not int.
	if t == 'int' && (value.starts_with('0x') || value.starts_with('0X')) && value.len > 10 {
		t = 'u64'
	}
	rt := v_wrapper_type(t, m) or { return false }
	b.writeln("@[export: '${cname}']")
	b.writeln('fn capi_const_${idx}() ${rt} {')
	if t == 'string' {
		b.writeln('\treturn &char(capi_rt_strdup(${value}))')
	} else if t == 'bool' {
		b.writeln('\treturn if ${value} { 1 } else { 0 }')
	} else if t == 'u64' {
		b.writeln('\treturn u64(${value})')
	} else {
		b.writeln('\treturn ${value}')
	}
	b.writeln('}')
	mut sig_type := 'int'
	if t == 'string' {
		sig_type = 'char *'
	} else if t == 'u64' {
		sig_type = 'uint64_t'
	}
	infos << ExportInfo{
		c_name:    cname
		signature: '${sig_type} ${cname}(void)'
		doc:       c.doc
	}
	return true
}
