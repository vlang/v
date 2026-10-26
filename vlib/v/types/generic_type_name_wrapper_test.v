module types

import v.flat

fn map_of(value Type) Type {
	return Type(Map{
		key_type:   new_type_reference(Type(string_))
		value_type: new_type_reference(value)
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
		ints := tc.parse_type(flat.source_type_id('${wrapped}[]int'))
		strings := tc.parse_type(flat.source_type_id('${wrapped}[]string'))
		assert !tc.slot_value_compatible(ints, strings), wrapped
		assert !tc.slot_value_compatible(strings, ints), wrapped
		assert tc.slot_value_compatible(ints, tc.parse_type(flat.source_type_id('${wrapped}[]int'))), wrapped
	}
}

fn test_generic_type_name_matches_keeps_wrapped_value_types() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	for prefix in ['', '&', 'mut &', '&mut ', '?', '!', 'chan ', 'shared ', '[]'] {
		assert !tc.generic_type_name_matches(flat.source_type_id('${prefix}map[string]int'), flat.source_type_id('${prefix}map[string]string')), prefix
		assert tc.generic_type_name_matches(flat.source_type_id('${prefix}map[string]int'), flat.source_type_id('${prefix}map[string]int')), prefix
		// An open generic value type still matches a concrete one.
		assert tc.generic_type_name_matches(flat.source_type_id('${prefix}map[string]T'), flat.source_type_id('${prefix}map[string]int')), prefix
	}
	for prefix in ['?', '!', '&', 'chan ', 'shared ', '...'] {
		assert !tc.generic_type_name_matches(flat.source_type_id('${prefix}[]int'), flat.source_type_id('${prefix}[]string')), prefix
		assert tc.generic_type_name_matches(flat.source_type_id('${prefix}[]Foo[T]'), flat.source_type_id('${prefix}[]Foo[int]')), prefix
	}
	// Wrapper layers are part of the type's identity.
	assert !tc.generic_type_name_matches(flat.source_type_id('?map[string]int'), flat.source_type_id('!map[string]int'))
	assert !tc.generic_type_name_matches(flat.source_type_id('&Foo[T]'), flat.source_type_id('Foo[int]'))
	assert tc.generic_type_name_matches(flat.source_type_id('?Foo[T]'), flat.source_type_id('?Foo[int]'))
	// The same holds inside generic arguments.
	assert !tc.generic_type_name_matches(flat.source_type_id('Box[chan []int]'), flat.source_type_id('Box[chan []string]'))
	assert !tc.generic_type_name_matches(flat.source_type_id('Box[?map[string]int]'), flat.source_type_id('Box[?map[string]string]'))
	assert tc.generic_type_name_matches(flat.source_type_id('Box[chan []T]'), flat.source_type_id('Box[chan []int]'))
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

fn test_generic_whole_spelling_ids_keep_file_resolution_and_trimmed_substrings() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.structs['one.Widget'] = []StructField{}
	tc.structs['two.Widget'] = []StructField{}
	tc.cur_file = 'one.v'
	tc.register_file_import('dep', 'one')
	tc.cur_file = 'two.v'
	tc.register_file_import('dep', 'two')
	for file, module_name in {
		'one.v': 'one'
		'two.v': 'two'
	} {
		tc.cur_file = file
		qualified := flat.source_type_id('${module_name}.Widget')
		for spelling in ['dep.Widget', ' dep.Widget '] {
			assert tc.generic_type_base_matches(flat.source_type_id(spelling), qualified)
			assert tc.generic_type_arg_matches(flat.source_type_id(spelling), qualified)
		}
		assert !tc.generic_type_arg_matches(flat.source_type_id('dep.Widget'), flat.source_type_id('other.Widget'))
		assert tc.generic_type_name_matches(flat.source_type_id('Box[?dep.Widget]'), flat.source_type_id('Box[?${module_name}.Widget]'))
	}
}
