module transform

import v.flat
import v.types

fn test_unsafe_block_argument_retains_mutability_after_lowering() {
	for mode in ['expression', 'typed', 'argument'] {
		for is_mut in [false, true] {
			mut a := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&a)
			mut t := new_transformer(mut a, &tc, map[string]bool{})
			value := a.add_node(flat.Node{ kind: .nil_literal })
			statement := t.make_expr_stmt(value)
			argument := t.make_block([statement])
			a.nodes[int(argument)].value = 'unsafe'
			a.nodes[int(argument)].is_mut = is_mut
			lowered := if mode == 'expression' {
				t.transform_block_expr(argument, a.nodes[int(argument)])
			} else if mode == 'typed' {
				t.transform_block_expr_for_type(argument, a.nodes[int(argument)], 'voidptr')?
			} else {
				t.transform_call_arg_for_param(argument, '&voidptr')
			}
			assert a.nodes[int(lowered)].kind == .block
			assert a.nodes[int(lowered)].is_mut == is_mut
		}
	}
}
