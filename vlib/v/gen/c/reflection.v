module c

import v.ast

// gen_reflection_function generates C code for reflection function metadata
fn (mut g Gen) gen_reflection_function(node ast.FnDecl) {
	if !g.has_reflection {
		return
	}

	if node.params.len == 0 {
		g.reflection_funcs.write_string('\tv__reflection__add_func(_SLIT("${g.cur_mod.name}"), _SLIT("${node.name}"), false);')
	} else {
		mut param_str := 'new_array_from_c_array(${node.params.len}, ${node.params.len}, sizeof(v__reflection__ReflectionFunctionArg), '
		param_str += '_MOV((v__reflection__ReflectionFunctionArg[${node.params.len}]){'
		for param in node.params {
			param_str += '((v__reflection__ReflectionFunctionArg){.name = _SLIT("${param.name}"),.typ = ${param.typ.idx()},}),'
		}
		param_str += '}))'

		g.reflection_funcs.write_string('\tv__reflection__add_func_with_args(_SLIT("${g.cur_mod.name}"), _SLIT("${node.name}"), false, ${param_str});')
	}
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
