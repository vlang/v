module types

import v.flat

fn array_return_type_test_call(mut a flat.FlatAst, method string) flat.NodeId {
	base := a.add_val(.ident, 'values')
	selector_start := a.begin_children()
	a.add_child(base)
	selector := a.add_node(flat.Node{
		kind:           .selector
		value:          method
		children_start: selector_start
		children_count: 1
	})
	call_start := a.begin_children()
	a.add_child(selector)
	return a.add_node(flat.Node{
		kind:           .call
		children_start: call_start
		children_count: 1
	})
}

fn test_array_builtin_return_types_and_custom_method_fallback() {
	expected := {
		'clone':               '[]int'
		'move':                '[]int'
		'reverse':             '[]int'
		'filter':              '[]int'
		'sorted':              '[]int'
		'sorted_with_compare': '[]int'
		'any':                 'bool'
		'all':                 'bool'
		'count':               'int'
		'sort':                'void'
		'last':                'int'
		'first':               'int'
		'pop':                 'int'
		'contains':            'bool'
		'repeat':              '[]int'
		'repeat_to_depth':     '[]int'
		'index':               'int'
		'join':                'string'
		'str':                 'string'
		'custom':              'string'
		'custom_with_compare': 'string'
	}
	for method, name in expected {
		mut a := flat.FlatAst.new()
		call := array_return_type_test_call(mut a, method)
		mut tc := TypeChecker.new(&a)
		tc.cur_scope.insert('values', Type(Array{ elem_type: new_type_reference(Type(int_)) }))
		tc.fn_ret_types['[]int.custom'] = builtin_string_type
		tc.fn_ret_types['[]int.custom_with_compare'] = builtin_string_type
		assert tc.resolve_type_nonliteral(call).name() == name, method
	}
}

fn test_array_alias_method_takes_priority_over_builtin_sorting() {
	mut a := flat.FlatAst.new()
	call := array_return_type_test_call(mut a, 'sorted_with_compare')
	mut tc := TypeChecker.new(&a)
	array := Type(Array{ elem_type: new_type_reference(Type(int_)) })
	tc.cur_scope.insert('values', Type(Alias{
		name_id:   flat.source_type_id('Numbers')
		base_type: new_type_reference(array)
	}))
	tc.fn_ret_types['Numbers.sorted_with_compare'] = builtin_string_type
	assert tc.resolve_type_nonliteral(call).name() == 'string'
}

fn test_fixed_array_methods_keep_their_receiver_gate() {
	for method in ['filter', 'clone', 'sort', 'sorted', 'sorted_with_compare', 'first', 'last',
		'pop'] {
		mut a := flat.FlatAst.new()
		call := array_return_type_test_call(mut a, method)
		mut tc := TypeChecker.new(&a)
		tc.cur_scope.insert('values', Type(ArrayFixed{
			elem_type: new_type_reference(Type(int_))
			len:       3
		}))
		typ := tc.resolve_type_nonliteral(call)
		if method == 'filter' || method == 'clone' {
			assert typ.name() == '[]int', method
		} else {
			assert typ is Unknown, method
		}
	}
}
