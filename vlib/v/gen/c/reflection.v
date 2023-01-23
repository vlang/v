module c

import v.ast

[inline]
fn (g Gen) gen_functionarg_array(type_name string, node ast.FnDecl) string {
	if node.params.len == 0 {
		return '__new_array_with_default(0, 0, sizeof(${type_name}), 0)'
	}
	mut out := 'new_array_from_c_array(${node.params.len}, ${node.params.len}, sizeof(${type_name}), '
	out += '_MOV((${type_name}[${node.params.len}]){'
	for param in node.params {
		out += '((${type_name}){.name = _SLIT("${param.name}"),.typ = ${param.typ.idx()},}),'
	}
	out += '}))'
	return out
}

// gen_reflection_function generates C code for reflection function metadata
fn (mut g Gen) gen_reflection_function(node ast.FnDecl) {
	if !g.has_reflection {
		return
	}
	mut arg_str := '((v__reflection__Function){'

	v_name := node.name.all_after_last('.')

	arg_str += '.mod_name = _SLIT("${node.mod}"),'
	arg_str += '.name = _SLIT("${v_name}"),'
	arg_str += '.full_name = _SLIT("${node.name}"),'
	arg_str += '.is_method = ${node.is_method},'
	arg_str += '.args = ${g.gen_functionarg_array('v__reflection__FunctionArg', node)},'
	arg_str += '.file=_SLIT("${node.file}"),'
	arg_str += '.line_start=${node.pos.line_nr},'
	arg_str += '.line_end=${node.pos.last_line},'
	arg_str += '.is_test=${node.is_test},'
	arg_str += '.is_variadic=${node.is_variadic},'
	arg_str += '.is_noreturn=${node.is_noreturn},'
	arg_str += '.return_typ=${node.return_type.idx()},'
	arg_str += '.receiver_typ=${node.receiver.typ.idx()}'
	arg_str += '})'
	g.reflection_funcs.write_string('\tv__reflection__add_func(${arg_str});\n')
}

// gen_reflection_data generates code to initilized V reflection metadata
fn (mut g Gen) gen_reflection_data() {
	// modules declaration
	for mod_name in g.table.modules {
		g.reflection_mods.write_string('\tv__reflection__add_module(_SLIT("${mod_name}"));')
	}
	// modules declaration
	g.writeln(g.reflection_mods.str())

	// funcs declaration
	g.writeln(g.reflection_funcs.str())
}
