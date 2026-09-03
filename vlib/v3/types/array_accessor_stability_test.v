module types

import v3.flat

fn add_array_accessor_spread_for_test(mut a flat.FlatAst, typ string) flat.NodeId {
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'values'
		typ: typ
	})
	return a.add_node(flat.Node{
		kind: .prefix
		value: '...'
		children_start: a.add_child(value)
		children_count: 1
	})
}

fn test_array_accessor_map_spread_stability_checks_key_and_value_clones() {
	mut a := flat.FlatAst.new()
	key_clone := add_array_accessor_spread_for_test(mut a, 'map[[1]Key]int')
	value_clone := add_array_accessor_spread_for_test(mut a, 'map[string]Mutator')
	builtin_clone := add_array_accessor_spread_for_test(mut a, 'map[string]int')
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.structs['Key'] = []StructField{}
	tc.structs['Mutator'] = []StructField{}
	tc.fn_ret_types['Key.clone'] = Type(Struct{
		name: 'Key'
	})
	tc.fn_ret_types['Mutator.clone'] = Type(Struct{
		name: 'Mutator'
	})

	assert !tc.array_accessor_borrow_sibling_is_stable(key_clone)
	assert !tc.array_accessor_borrow_sibling_is_stable(value_clone)
	assert tc.array_accessor_borrow_sibling_is_stable(builtin_clone)
}
