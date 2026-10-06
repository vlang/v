module types

import v.flat

fn test_scalar_parser_preserves_interner_order_and_context_cache_entries() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	mut expected := TypeChecker.new(&a)
	tc.set_fresh_type_cache(true)
	tc.cur_module = 'scalar'
	tc.cur_file = 'one.v'
	names := ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'i128', 'u128',
		'f32', 'f64', 'string', 'char', 'rune', 'isize', 'usize', 'uint', 'void', 'voidptr', 'charptr',
		'byteptr', 'nil', 'none']
	for name in names {
		expected_id, expected_type := expected.intern_type(builtin_type_value(name))
		parsed := tc.parse_type(name)
		actual_id, _ := tc.intern_type(parsed)
		assert actual_id == expected_id
		assert semantic_types_equal(parsed, expected_type)
	}
	assert tc.type_count() == expected.type_count()
	assert tc.type_cache.parse_entries.len == names.len
	assert tc.type_cache_stats().parse_misses == names.len
	for name in names {
		assert semantic_types_equal(tc.parse_type(name), builtin_type_value(name))
	}
	assert tc.type_cache_stats().parse_hits == names.len
	tc.cur_file = 'two.v'
	for name in names {
		assert semantic_types_equal(tc.parse_type(name), builtin_type_value(name))
	}
	assert tc.type_count() == expected.type_count()
	assert tc.type_cache.parse_entries.len == 2 * names.len
	assert tc.type_cache_stats().parse_misses == 2 * names.len
}

fn test_scalar_parser_keeps_recursive_alias_and_type_parameter_precedence() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_fresh_type_cache(true)
	tc.type_cache.alias_parse_stack << 'u64'
	recursive := tc.parse_type('u64')
	assert recursive is Alias
	assert recursive.name == 'u64'
	assert recursive.base_type is Unknown
	assert tc.type_cache.parse_entries.len == 0
	assert tc.type_count() == 0
	tc.type_cache.alias_parse_stack.clear()
	assert tc.parse_type('u64') == builtin_type_value('u64')
	tc.type_param_texts['u64'] = 'string'
	assert tc.parse_type('u64') is String
	assert tc.type_cache.parse_entries.len == 1
}

fn test_scalar_parser_keeps_builtin_nominal_precedence_and_array_map_context() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_fresh_type_cache(true)
	tc.cur_module = 'shadow'
	tc.type_aliases['u64'] = 'string'
	tc.type_aliases['shadow.u64'] = 'string'
	tc.sum_types['u64'] = ['string', 'int']
	tc.interface_names['u64'] = true
	assert tc.parse_type('u64') == builtin_type_value('u64')
	assert tc.parse_type('&u64') is Pointer
	assert tc.parse_type('?u64') is OptionType
	tc.has_builtins = true
	tc.structs['array'] = []StructField{}
	assert tc.parse_type('array') is Struct
	assert tc.parse_type('map') is Struct
	tc.has_builtins = false
	tc.set_fresh_type_cache(true)
	assert tc.parse_type('array') is Array
	assert tc.parse_type('map') is Unknown
}
