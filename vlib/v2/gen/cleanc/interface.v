// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast

struct InterfaceMethodInfo {
	name           string // method name (e.g., "draw")
	cast_signature string // function pointer cast (e.g., "int (*)(void*)")
	ret_type       string
	param_types    []string
}

struct InterfaceWrapperSpec {
	fn_name       string
	concrete_type string
	method        InterfaceMethodInfo
}

fn interface_type_id_for_name(name string) int {
	match name {
		'None__' { return 1 }
		'Error' { return 2 }
		'MessageError' { return 3 }
		else { return 0 }
	}
}

fn interface_wrapper_name(iface_name string, concrete_type string, method_name string) string {
	return '__iface_wrap_' + mangle_alias_component(iface_name) + '_' +
		mangle_alias_component(concrete_type) + '_' + method_name
}

fn (mut g Gen) collect_interface_wrapper_specs() {
	if g.interface_wrapper_specs.len > 0 {
		return
	}
	mut iface_names := g.interface_methods.keys()
	iface_names.sort()
	mut fn_names := g.fn_param_is_ptr.keys()
	fn_names.sort()
	for iface_name in iface_names {
		methods := g.interface_methods[iface_name]
		for method in methods {
			suffix := '__${method.name}'
			for fn_name in fn_names {
				if !fn_name.ends_with(suffix) {
					continue
				}
				ptr_params := g.fn_param_is_ptr[fn_name] or { continue }
				if ptr_params.len == 0 || ptr_params[0] {
					// Pointer receivers can be called directly through void*.
					continue
				}
				fn_params := g.fn_param_types[fn_name] or { continue }
				if fn_params.len != method.param_types.len + 1 {
					continue
				}
				if fn_ret := g.fn_return_types[fn_name] {
					if fn_ret != method.ret_type {
						continue
					}
				}
				concrete_type := fn_name[..fn_name.len - suffix.len]
				receiver_type := fn_params[0].trim_right('*')
				if receiver_type != concrete_type {
					continue
				}
				wrapper_name := interface_wrapper_name(iface_name, concrete_type, method.name)
				if wrapper_name in g.interface_wrapper_specs {
					continue
				}
				g.interface_wrapper_specs[wrapper_name] = InterfaceWrapperSpec{
					fn_name:       fn_name
					concrete_type: concrete_type
					method:        method
				}
			}
		}
	}
}

fn (mut g Gen) emit_interface_method_wrapper_decls() {
	mut names := g.interface_wrapper_specs.keys()
	names.sort()
	for name in names {
		spec := g.interface_wrapper_specs[name]
		g.sb.write_string('static ${spec.method.ret_type} ${name}(void* _obj')
		for i, param_type in spec.method.param_types {
			g.sb.write_string(', ${param_type} _arg${i}')
		}
		g.sb.writeln(');')
	}
	if names.len > 0 {
		g.sb.writeln('')
	}
}

fn (mut g Gen) emit_interface_method_wrapper_body(name string, spec InterfaceWrapperSpec) {
	g.sb.write_string('static ${spec.method.ret_type} ${name}(void* _obj')
	for i, param_type in spec.method.param_types {
		g.sb.write_string(', ${param_type} _arg${i}')
	}
	g.sb.writeln(') {')
	if spec.method.ret_type != 'void' {
		g.sb.write_string('\treturn ')
	} else {
		g.sb.write_string('\t')
	}
	g.sb.write_string('${spec.fn_name}(*(((${spec.concrete_type}*)_obj))')
	for i in 0 .. spec.method.param_types.len {
		g.sb.write_string(', _arg${i}')
	}
	g.sb.writeln(');')
	g.sb.writeln('}')
	g.sb.writeln('')
}

fn (mut g Gen) emit_needed_interface_method_wrappers() {
	mut names := g.needed_interface_wrappers.keys()
	names.sort()
	if names.len == 0 {
		return
	}
	for name in names {
		spec := g.interface_wrapper_specs[name] or { continue }
		g.emit_interface_method_wrapper_body(name, spec)
	}
}

fn (mut g Gen) collect_ierror_wrapper_bases() {
	if g.ierror_wrapper_bases.len > 0 {
		return
	}
	for fn_name, ret_type in g.fn_return_types {
		if !fn_name.ends_with('__msg') || ret_type != 'string' {
			continue
		}
		base := fn_name[..fn_name.len - '__msg'.len]
		if base == '' || !is_c_identifier_like(base) {
			continue
		}
		g.ierror_wrapper_bases[base] = true
	}
}

fn (mut g Gen) mark_needed_ierror_wrapper_from_ident(name string) {
	base := ierror_wrapper_base_from_ident(name)
	if base == '' {
		return
	}
	g.needed_ierror_wrapper_bases[base] = true
}

fn (g &Gen) should_emit_ierror_wrappers() bool {
	body_ierror_key := 'body_IError'
	body_builtin_ierror_key := 'body_builtin__IError'
	return body_ierror_key in g.emitted_types || body_builtin_ierror_key in g.emitted_types
}

fn (mut g Gen) emit_ierror_wrapper_decls() {
	if !g.should_emit_ierror_wrappers() {
		return
	}
	g.collect_ierror_wrapper_bases()
	mut bases := g.ierror_wrapper_bases.keys()
	bases.sort()
	for base in bases {
		g.sb.writeln('static string IError_${base}_type_name_wrapper(void* _obj);')
		g.sb.writeln('static string IError_${base}_msg_wrapper(void* _obj);')
		g.sb.writeln('static int IError_${base}_code_wrapper(void* _obj);')
	}
	if bases.len > 0 {
		g.sb.writeln('')
	}
}

fn (mut g Gen) emit_ierror_wrapper_body(base string) {
	type_label := if base.contains('__') { base.all_after_last('__') } else { base }
	g.sb.writeln('static string IError_${base}_type_name_wrapper(void* _obj) {')
	g.sb.writeln('\t(void)_obj;')
	g.sb.writeln('\treturn ${c_static_v_string_expr(type_label)};')
	g.sb.writeln('}')
	g.sb.writeln('static string IError_${base}_msg_wrapper(void* _obj) {')
	g.sb.writeln('\treturn ${base}__msg(*(${base}*)_obj);')
	g.sb.writeln('}')
	g.sb.writeln('static int IError_${base}_code_wrapper(void* _obj) {')
	code_fn := '${base}__code'
	error_code_fn := 'Error__code'
	if code_fn in g.fn_return_types {
		g.sb.writeln('\treturn ${base}__code(*(${base}*)_obj);')
	} else if error_code_fn in g.fn_return_types {
		g.sb.writeln('\treturn Error__code(*(Error*)_obj);')
	} else {
		g.sb.writeln('\t(void)_obj;')
		g.sb.writeln('\treturn 1;')
	}
	g.sb.writeln('}')
}

fn (mut g Gen) emit_needed_ierror_wrappers() {
	if !g.should_emit_ierror_wrappers() {
		return
	}
	mut bases := g.needed_ierror_wrapper_bases.keys()
	bases.sort()
	if bases.len == 0 {
		return
	}
	mut emitted_any := false
	for base in bases {
		if base !in g.ierror_wrapper_bases {
			continue
		}
		g.emit_ierror_wrapper_body(base)
		emitted_any = true
	}
	if emitted_any {
		g.sb.writeln('')
	}
}

fn (mut g Gen) get_interface_name(node ast.InterfaceDecl) string {
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${node.name}'
	}
	return node.name
}

fn (mut g Gen) gen_interface_decl(node ast.InterfaceDecl) {
	name := g.get_interface_name(node)
	body_key := 'body_${name}'
	if body_key in g.emitted_types {
		return
	}
	g.emitted_types[body_key] = true

	g.sb.writeln('struct ${name} {')
	g.sb.writeln('\tvoid* _object;')
	g.sb.writeln('\tint _type_id;')
	mut has_type_name := false
	for field in node.fields {
		if field.name == 'type_name' {
			has_type_name = true
			break
		}
	}
	if !has_type_name {
		g.sb.writeln('\tstring (*type_name)(void*);')
	}
	// Generate function pointers for each method
	mut methods := []InterfaceMethodInfo{}
	for field in node.fields {
		if fn_type := g.get_fn_type_from_expr(field.typ) {
			mut ret := 'void'
			if fn_type.return_type !is ast.EmptyExpr {
				ret = g.expr_type_to_c(fn_type.return_type)
			}
			mut param_types := []string{}
			g.sb.write_string('\t${ret} (*${field.name})(void*')
			mut cast_sig := '${ret} (*)(void*'
			for param in fn_type.params {
				g.sb.write_string(', ')
				t := g.expr_type_to_c(param.typ)
				g.sb.write_string(t)
				cast_sig += ', ${t}'
				param_types << t
			}
			g.sb.writeln(');')
			cast_sig += ')'
			methods << InterfaceMethodInfo{
				name:           field.name
				cast_signature: cast_sig
				ret_type:       ret
				param_types:    param_types
			}
		} else {
			// Regular field
			t := g.expr_type_to_c(field.typ)
			g.sb.writeln('\t${t} ${field.name};')
		}
	}
	g.interface_methods[name] = methods
	g.sb.writeln('};')
	g.sb.writeln('')
}
