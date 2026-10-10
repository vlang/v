module transform

import v.flat

fn array_selector_test_node(mut a flat.FlatAst, receiver flat.NodeId, field string) flat.NodeId {
	start := a.children.len
	a.children << receiver
	return a.add_node(flat.Node{
		kind:           .selector
		value:          field
		children_start: start
		children_count: 1
	})
}

fn test_builtin_array_fields_do_not_inherit_unrelated_struct_field_types() {
	for receiver_type in ['[]u8', '&[]u8', 'array', '&array'] {
		mut a := flat.FlatAst.new()
		receiver := a.add_val(.ident, 'items')
		t := Transformer{
			a:             &a
			var_types:     [VarTypeBinding{ name: 'items', typ: receiver_type }]
			unique_fields: {
				'data':         'int'
				'offset':       'bool'
				'len':          'string'
				'cap':          'string'
				'flags':        'string'
				'element_size': 'bool'
			}
		}
		for field, expected in {
			'data':         'voidptr'
			'offset':       'int'
			'len':          'int'
			'cap':          'int'
			'flags':        'ArrayFlags'
			'element_size': 'int'
		} {
			selector := array_selector_test_node(mut a, receiver, field)
			assert t.node_type(selector) == expected
		}
	}
}

fn test_builtin_array_data_is_passed_as_a_pointer_value_to_v_functions() {
	mut a := flat.FlatAst.new()
	receiver := a.add_val(.ident, 'items')
	selector := array_selector_test_node(mut a, receiver, 'data')
	mut t := Transformer{
		a:             &a
		var_types:     [VarTypeBinding{ name: 'items', typ: '[]u8' }]
		unique_fields: {
			'data': 'int'
		}
	}
	argument := t.transform_call_arg_for_param(selector, 'voidptr')
	assert a.node(argument).kind == .selector
	assert a.node(argument).value == 'data'
}
