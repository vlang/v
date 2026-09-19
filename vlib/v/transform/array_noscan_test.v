module transform

import v.flat
import v.types

fn test_array_new_selects_noscan_for_scalar_elements() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['ScalarAlias'] = 'f64'
	tc.type_aliases['ScalarAliasChain'] = 'ScalarAlias'
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	len_expr := t.make_int_literal(3)
	cap_expr := t.make_int_literal(7)
	for elem_type in ['f64', 'f32', 'int', 'u8', 'bool', 'char', 'rune', 'isize', 'usize',
		'ScalarAlias', 'ScalarAliasChain'] {
		id := t.make_array_new_call(elem_type, len_expr, cap_expr)
		call := a.nodes[int(id)]
		assert call.kind == .call
		assert call.typ == '[]${elem_type}'
		assert a.child_node(&call, 0).value == '__new_array_noscan'
		// __new_array_noscan takes len/cap/size, unlike array_new's size/len/cap.
		assert a.child(&call, 1) == len_expr
		assert a.child(&call, 2) == cap_expr
		assert a.child_node(&call, 3).kind == .sizeof_expr
		assert a.child_node(&call, 3).value == elem_type
	}
	assert t.used_fns['__new_array_noscan']
	assert array_element_can_use_noscan(types.Enum{ name: 'ScalarEnum' })
}

fn test_array_new_keeps_pointer_bearing_and_unknown_elements_scanned() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['PointerAlias'] = '&f64'
	tc.structs['PointerValue'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	len_expr := t.make_int_literal(2)
	cap_expr := t.make_int_literal(5)
	for elem_type in ['[]f64', 'string', '&f64', 'voidptr', 'PointerAlias', 'PointerValue',
		'map[string]int', '?f64', 'fn ()', 'shared int', 'UnresolvedElement'] {
		id := t.make_array_new_call(elem_type, len_expr, cap_expr)
		call := a.nodes[int(id)]
		assert a.child_node(&call, 0).value == 'array_new'
		assert a.child_node(&call, 1).kind == .sizeof_expr
		expected_size_type := if elem_type == 'shared int' {
			'&void'
		} else {
			elem_type
		}
		assert a.child_node(&call, 1).value == expected_size_type
		assert a.child(&call, 2) == len_expr
		assert a.child(&call, 3) == cap_expr
	}
	assert !t.used_fns['__new_array_noscan']
}
