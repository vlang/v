module types

import v.flat

fn string_literal_selector(mut a flat.FlatAst, value string, field string) flat.NodeId {
	literal := a.add_node(flat.Node{
		kind:  .string_literal
		value: value
	})
	children_start := a.begin_children()
	a.add_child(literal)
	return a.add_node(flat.Node{
		kind:           .selector
		value:          field
		children_start: children_start
		children_count: 1
	})
}

fn test_string_literal_str_field_shadows_method() {
	for value in ['', 'hello'] {
		for fast in [false, true] {
			mut a := flat.FlatAst.new()
			selector := string_literal_selector(mut a, value, 'str')
			mut tc := TypeChecker.new(&a)
			tc.valid_resolution_fast = fast
			field_type := Type(Pointer{
				base_type: Type(u8_)
			})
			tc.structs['string'] = [
				StructField{
					name: 'str'
					typ:  field_type
				},
			]
			tc.fn_param_types['string.str'] = [Type(string_)]
			tc.fn_ret_types['string.str'] = Type(string_)

			typ := tc.selector_type(selector, *a.node(selector)) or {
				panic('missing string field type')
			}
			assert typ is Pointer
			assert typ.name() == '&u8'
			tc.check_selector(selector, *a.node(selector))
			assert tc.errors.len == 0, tc.errors.str()
			assert tc.resolve_type(selector).name() == '&u8'
		}
	}
}

fn test_string_literal_method_value_without_a_matching_field() {
	mut a := flat.FlatAst.new()
	selector := string_literal_selector(mut a, 'hello', 'to_upper')
	mut tc := TypeChecker.new(&a)
	tc.structs['string'] = [
		StructField{
			name: 'str'
			typ:  Type(Pointer{
				base_type: Type(u8_)
			})
		},
	]
	tc.fn_param_types['string.to_upper'] = [Type(string_)]
	tc.fn_ret_types['string.to_upper'] = Type(string_)

	typ := tc.selector_type(selector, *a.node(selector)) or {
		panic('missing string method type')
	}
	assert typ is FnType
	if typ is FnType {
		assert typ.params.len == 0
		assert typ.return_type is String
	}
}
