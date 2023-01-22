module c

import v.ast

// gen_reflection_function generates C code for reflection function metadata
fn (mut g Gen) gen_reflection_function(node ast.FnDecl) {
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
