module transform

import v.flat
import v.types

fn builtin_shape_call_name(a &flat.FlatAst, id flat.NodeId) string {
	node := a.nodes[int(id)]
	assert node.kind == .call
	return a.nodes[int(a.child(&node, 0))].value
}

fn test_builtin_call_shape_keeps_ordinary_bare_and_indexed_function_calls() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.validating_generic_spec = true
	for name in ['ordinary', 'has', 'free', 'clone', 'type_name', 'from', 'from_string'] {
		for arity in 0 .. 3 {
			mut args := []flat.NodeId{}
			for i in 0 .. arity {
				args << t.make_int_literal(i)
			}
			call := t.make_call_expr_typed(t.make_ident(name), args, flat.source_type_int)
			node := a.nodes[int(call)]
			nodes_before := a.nodes.len
			pending_before := t.pending_stmts.len
			assert t.try_lower_builtin_call(call, node) == none
			assert a.nodes.len == nodes_before
			assert t.pending_stmts.len == pending_before
		}
	}
	indexed := t.make_index(t.make_ident('ordinary'), t.make_ident('int'), u32(0))
	call := t.make_call_expr_typed(indexed, [t.make_int_literal(1)], flat.source_type_int)
	assert t.try_lower_builtin_call(call, a.nodes[int(call)]) == none
}

fn test_builtin_call_shape_preserves_primitive_named_and_generic_casts() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.begin_sparse_transform_node_caches(0)
	tc.structs[flat.source_type_id('Target')] = []types.StructField{}
	tc.structs[flat.source_type_id('Box')] = []types.StructField{}
	tc.structs[flat.source_type_id('Box[int]')] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for target in ['int', 'Target', 'Box[int]'] {
		call := t.make_call_expr_typed(t.make_ident(target), [t.make_int_literal(1)],
			flat.source_type_id(target))
		if target == 'Target' {
			tc.sparse_resolved_call_names[int(call)] = target
		}
		lowered := t.try_lower_builtin_call(call, a.nodes[int(call)]) or {
			panic('cast ${target} was skipped')
		}
		assert a.nodes[int(lowered)].kind == .cast_expr
		assert a.nodes[int(lowered)].source_value_id() == flat.source_type_id(target)
	}
	// A local with the type's spelling must keep the resolved-name cast guard.
	t.set_var_type('Target', flat.source_type_id('fn (int) int'))
	shadowed := t.make_call_expr_typed(t.make_ident('Target'), [t.make_int_literal(2)],
		flat.source_type_int)
	tc.sparse_resolved_call_names[int(shadowed)] = 'Target'
	assert t.try_lower_builtin_call(shadowed, a.nodes[int(shadowed)]) == none
}

fn test_builtin_call_shape_preserves_bare_and_qualified_flag_default_value() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for qualified in [false, true] {
		callee := if qualified {
			t.make_selector(t.make_ident('builtin'), 'flag_default_value', u32(0))
		} else {
			t.make_ident('flag_default_value')
		}
		arg := t.make_string_literal('value')
		call := t.make_call_expr_typed(callee, [arg], flat.source_type_string)
		lowered := t.try_lower_builtin_call(call, a.nodes[int(call)]) or {
			panic('flag_default_value was skipped')
		}
		assert builtin_shape_call_name(&a, lowered) == 'string__plus'
		outer := a.nodes[int(lowered)]
		inner := a.child(&outer, 1)
		assert builtin_shape_call_name(&a, inner) == 'string__plus'
		inner_node := a.nodes[int(inner)]
		escaped := a.child(&inner_node, 2)
		assert builtin_shape_call_name(&a, escaped) == 'escape_default_string'
	}
}

fn test_builtin_call_shape_preserves_static_assoc_with_generic_index_wrapper() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Box'] = StructInfo{ name: flat.source_type_id('Box'), module: 'main' }
	static_name := flat.encode_static_type_method_name('Box', 'make')
	t.fn_ret_types.set_id(static_name, flat.source_type_int)
	for generic in [false, true] {
		selector := t.make_selector(t.make_ident('Box'), 'make', u32(0))
		callee := if generic {
			t.make_index(selector, t.make_ident('int'), u32(0))
		} else {
			selector
		}
		call := t.make_call_expr_typed(callee, []flat.NodeId{}, flat.source_type_int)
		lowered := t.try_lower_builtin_call(call, a.nodes[int(call)]) or {
			panic('static associated call was skipped')
		}
		assert builtin_shape_call_name(&a, lowered) == static_name
		assert a.nodes[int(lowered)].children_count == 1
		assert a.nodes[int(lowered)].call_type_args() == if generic {
			[flat.source_type_int]
		} else {
			[]u32{}
		}
	}
}

fn test_builtin_call_shape_preserves_selector_folding_and_c_interop() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	selector := t.make_selector(t.make_string_literal('aBc'), 'to_upper', flat.source_type_string)
	call := t.make_call_expr_typed(selector, []flat.NodeId{}, flat.source_type_string)
	lowered := t.try_lower_builtin_call(call, a.nodes[int(call)]) or {
		panic('literal selector call was skipped')
	}
	assert a.nodes[int(lowered)].kind == .string_literal
	assert a.nodes[int(lowered)].value == 'ABC'
	c_callee := t.make_selector(t.make_ident('C'), 'printf', u32(0))
	c_call := t.make_call_expr_typed(c_callee, []flat.NodeId{}, flat.source_type_int)
	assert t.try_lower_builtin_call(c_call, a.nodes[int(c_call)]) == none
}

fn test_builtin_call_shape_preserves_flag_enum_zero() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.enum_names[flat.source_type_id('Flags')] = true
	tc.flag_enums[flat.source_type_id('Flags')] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	selector := t.make_selector(t.make_ident('Flags'), 'zero', u32(0))
	call := t.make_call_expr_typed(selector, []flat.NodeId{}, flat.source_type_id('Flags'))
	lowered := t.try_lower_builtin_call(call, a.nodes[int(call)]) or {
		panic('flag enum zero was skipped')
	}
	assert a.nodes[int(lowered)].kind == .cast_expr
	assert a.nodes[int(lowered)].source_value_id() == flat.source_type_id('Flags')
}
