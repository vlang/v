module c

import v.ast
import v.util

// gen_empty_array generates code for empty array
[inline]
fn (g Gen) gen_empty_array(type_name string) string {
	return '__new_array_with_default(0, 0, sizeof(${type_name}), 0)'
}

// gen_functionarg_array generates the code for functionarg argument
[inline]
fn (g Gen) gen_functionarg_array(type_name string, node ast.FnDecl) string {
	if node.params.len == 0 {
		return g.gen_empty_array(type_name)
	}
	mut out := 'new_array_from_c_array(${node.params.len}, ${node.params.len}, sizeof(${type_name}), '
	out += '_MOV((${type_name}[${node.params.len}]){'
	for param in node.params {
		out += '((${type_name}){.name = _SLIT("${param.name}"),.typ = ${param.typ.idx()},}),'
	}
	out += '}))'
	return out
}

// gen_functionarg_array generates the code for functionarg argument
[inline]
fn (g Gen) gen_function_array(nodes []ast.FnDecl) string {
	type_name := 'v__reflection__Function'

	if nodes.len == 0 {
		return g.gen_empty_array(type_name)
	}

	mut out := 'new_array_from_c_array(${nodes.len}, ${nodes.len}, sizeof(${type_name}), '
	out += '_MOV((${type_name}[${nodes.len}]){'
	for method in nodes {
		out += g.gen_reflection_fndecl(method)
		out += ','
	}
	out += '}))'
	return out
}

[inline]
fn (g Gen) gen_reflection_enum_fields(fields []ast.EnumField) string {
	if fields.len == 0 {
		return g.gen_empty_array('v__reflection__EnumField')
	}
	mut out := 'new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(v__reflection__EnumField), '
	out += '_MOV((v__reflection__EnumField[${fields.len}]){'
	for field in fields {
		out += '((v__reflection__EnumField){.name=_SLIT("${field.name}")}),'
	}
	out += '}))'
	return out
}

[inline]
fn (g Gen) gen_reflection_fndecl(node ast.FnDecl) string {
	mut arg_str := '((v__reflection__Function){'
	v_name := node.name.all_after_last('.')
	arg_str += '.mod_name = _SLIT("${node.mod}"),'
	arg_str += '.name = _SLIT("${v_name}"),'
	arg_str += '.args = ${g.gen_functionarg_array('v__reflection__FunctionArg', node)},'
	arg_str += '.file=_SLIT("${util.cescaped_path(node.file)}"),'
	arg_str += '.line_start=${node.pos.line_nr},'
	arg_str += '.line_end=${node.pos.last_line},'
	arg_str += '.is_variadic=${node.is_variadic},'
	arg_str += '.return_typ=${node.return_type.idx()},'
	arg_str += '.receiver_typ=${node.receiver.typ.idx()}'
	arg_str += '})'
	return arg_str
}

fn (g Gen) gen_reflection_sym(tsym ast.TypeSymbol) string {
	kind_name := if tsym.kind in [.none_, .struct_, .enum_, .interface_] {
		tsym.kind.str() + '_'
	} else {
		tsym.kind.str()
	}
	return '(v__reflection__TypeSymbol){.name=_SLIT("${tsym.name}"),.idx=${tsym.idx},.parent_idx=${tsym.parent_idx},.language=_SLIT("${tsym.language}"),.kind=v__ast__Kind__${kind_name}}'
}

// gen_reflection_function generates C code for reflection function metadata
fn (mut g Gen) gen_reflection_function(node ast.FnDecl) {
	if !g.has_reflection {
		return
	}
	func_struct := g.gen_reflection_fndecl(node)
	g.reflection_funcs.write_string('\tv__reflection__add_func(${func_struct});\n')
}

// gen_reflection_data generates code to initilized V reflection metadata
fn (mut g Gen) gen_reflection_data() {
	// modules declaration
	for mod_name in g.table.modules {
		g.reflection_others.write_string('\tv__reflection__add_module(_SLIT("${mod_name}"));\n')
	}

	// enum declaration
	for full_name, enum_ in g.table.enum_decls {
		name := full_name.all_after_last('.')
		fields := g.gen_reflection_enum_fields(enum_.fields)
		g.reflection_others.write_string('\tv__reflection__add_enum((v__reflection__Enum){.name=_SLIT("${name}"),.is_pub=${enum_.is_pub},.is_flag=${enum_.is_flag},.typ=${enum_.typ.idx()},.line_start=${enum_.pos.line_nr},.line_end=${enum_.pos.last_line},.fields=${fields}});\n')
	}

	// types declaration
	for full_name, idx in g.table.type_idxs {
		tsym := g.table.sym_by_idx(idx)
		name := full_name.all_after_last('.')
		sym := g.gen_reflection_sym(tsym)
		g.reflection_others.write_string('\tv__reflection__add_type((v__reflection__Type){.name=_SLIT("${name}"),.idx=${idx},.sym=${sym}});\n')
	}

	// interface declaration
	for _, idecl in g.table.interfaces {
		name := idecl.name.all_after_last('.')
		methods := g.gen_function_array(idecl.methods)
		g.reflection_others.write_string('\tv__reflection__add_interface((v__reflection__Interface){.name=_SLIT("${name}"),.typ=${idecl.typ.idx()},.is_pub=${idecl.is_pub},.methods=${methods}});\n')
	}

	// type symbols declaration
	for _, tsym in g.table.type_symbols {
		sym := g.gen_reflection_sym(tsym)
		g.reflection_others.write_string('\tv__reflection__add_type_symbol(${sym});\n')
	}

	// funcs meta info filling
	g.writeln(g.reflection_funcs.str())

	// others meta info filling
	g.writeln(g.reflection_others.str())
}
