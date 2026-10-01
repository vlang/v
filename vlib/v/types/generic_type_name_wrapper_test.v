module types

import v.flat

fn map_of(value Type) Type {
	return Type(Map{
		key_type:   Type(string_)
		value_type: value
	})
}

fn test_wrapped_maps_with_different_value_types_are_not_slot_compatible() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	opt_int_map := Type(OptionType{
		base_type: map_of(Type(int_))
	})
	opt_string_map := Type(OptionType{
		base_type: map_of(Type(string_))
	})
	assert !tc.slot_value_compatible(opt_int_map, opt_string_map)
	assert !tc.slot_value_compatible(opt_string_map, opt_int_map)
	assert tc.slot_value_compatible(opt_int_map, Type(OptionType{
		base_type: map_of(Type(int_))
	}))

	chan_int_map := Type(Channel{
		elem_type: map_of(Type(int_))
	})
	chan_string_map := Type(Channel{
		elem_type: map_of(Type(string_))
	})
	assert !tc.slot_value_compatible(chan_int_map, chan_string_map)
	assert !tc.slot_value_compatible(chan_string_map, chan_int_map)
	assert tc.slot_value_compatible(chan_int_map, Type(Channel{
		elem_type: map_of(Type(int_))
	}))

	res_int_map := Type(ResultType{
		base_type: map_of(Type(int_))
	})
	res_string_map := Type(ResultType{
		base_type: map_of(Type(string_))
	})
	assert !tc.slot_value_compatible(res_int_map, res_string_map)
	assert tc.slot_value_compatible(res_int_map, Type(ResultType{
		base_type: map_of(Type(int_))
	}))
}

fn test_wrapped_arrays_with_different_element_types_are_not_slot_compatible() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	for wrapped in ['?', 'chan '] {
		ints := tc.parse_type('${wrapped}[]int')
		strings := tc.parse_type('${wrapped}[]string')
		assert !tc.slot_value_compatible(ints, strings), wrapped
		assert !tc.slot_value_compatible(strings, ints), wrapped
		assert tc.slot_value_compatible(ints, tc.parse_type('${wrapped}[]int')), wrapped
	}
}

fn test_generic_type_name_matches_keeps_wrapped_value_types() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	for prefix in ['', '&', 'mut &', '&mut ', '?', '!', 'chan ', 'shared ', '[]'] {
		assert !tc.generic_type_name_matches('${prefix}map[string]int', '${prefix}map[string]string'), prefix
		assert tc.generic_type_name_matches('${prefix}map[string]int', '${prefix}map[string]int'), prefix
		// An open generic value type still matches a concrete one.
		assert tc.generic_type_name_matches('${prefix}map[string]T', '${prefix}map[string]int'), prefix
	}
	for prefix in ['?', '!', '&', 'chan ', 'shared ', '...'] {
		assert !tc.generic_type_name_matches('${prefix}[]int', '${prefix}[]string'), prefix
		assert tc.generic_type_name_matches('${prefix}[]Foo[T]', '${prefix}[]Foo[int]'), prefix
	}
	// Wrapper layers are part of the type's identity.
	assert !tc.generic_type_name_matches('?map[string]int', '!map[string]int')
	assert !tc.generic_type_name_matches('&Foo[T]', 'Foo[int]')
	assert tc.generic_type_name_matches('?Foo[T]', '?Foo[int]')
	// The same holds inside generic arguments.
	assert !tc.generic_type_name_matches('Box[chan []int]', 'Box[chan []string]')
	assert !tc.generic_type_name_matches('Box[?map[string]int]', 'Box[?map[string]string]')
	assert tc.generic_type_name_matches('Box[chan []T]', 'Box[chan []int]')
}

fn test_generic_type_application_parts_keeps_map_value_behind_wrappers() {
	for prefix in ['&', 'mut &', '&mut ', '?', '!', 'chan ', 'shared '] {
		base, args, ok := generic_type_application_parts('${prefix}map[string]int')
		assert ok, prefix
		assert base == '${prefix}map', prefix
		assert args == ['string', 'int'], prefix
	}
	_, _, ok := generic_type_application_parts('map[string]')
	assert !ok
}
