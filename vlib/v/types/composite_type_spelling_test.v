module types

import v.flat

fn assert_composite_spelling(typ Type, expected string) {
	assert typ.name() == expected
	first := typ.source_type_id()
	assert first == flat.source_type_id(expected)
	assert flat.source_type_name(first) == expected
	for _ in 0 .. 3 {
		assert typ.source_type_id() == first
		assert typ.name() == expected
	}
}

fn test_composite_spelling_ids_preserve_wrapper_order_and_fixed_length_text() {
	array := Type(Array{ elem_type: &builtin_int_type })
	optional := Type(OptionType{ base_type: &array })
	pointer := Type(Pointer{ base_type: &optional })
	assert_composite_spelling(pointer, '&?[]int')
	assert_composite_spelling(Type(ResultType{ base_type: &pointer }), '!&?[]int')
	assert_composite_spelling(Type(ArrayFixed{ elem_type: &optional, len: 3 }), '[3]?[]int')
	assert_composite_spelling(Type(ArrayFixed{
		elem_type: &builtin_int_type
		len:       3
		len_expr:  ' n + 1 '
	}), '[ n + 1 ]int')
	assert_composite_spelling(Type(ArrayFixed{
		elem_type: &builtin_int_type
		len:       3
		len_expr:  'n+1'
	}), '[n+1]int')
	assert_composite_spelling(Type(Map{
		key_type:   &builtin_string_type
		value_type: &optional
	}), 'map[string]?[]int')
}

fn test_composite_spelling_ids_preserve_mutable_and_variadic_precedence() {
	int_pointer := Type(Pointer{ base_type: &builtin_int_type })
	strings := Type(Array{ elem_type: &builtin_string_type })
	callback := Type(FnType{
		params:      [FnParam{ typ: &int_pointer, is_mut: true }, FnParam{ typ: &strings }]
		return_type: &builtin_string_type
		is_variadic: true
	})
	assert_composite_spelling(callback, 'fn(mut int, ...string) string')
	assert_composite_spelling(Type(FnType{
		params:      [FnParam{ typ: &int_pointer, is_mut: true }]
		is_variadic: true
	}), 'fn(mut int)')
	assert_composite_spelling(Type(FnType{
		params:      [FnParam{ typ: &strings, is_mut: true }]
		is_variadic: true
	}), 'fn(mut ...string)')
	assert_composite_spelling(Type(FnType{
		params: [FnParam{ typ: &callback }]
	}), 'fn(fn(mut int, ...string) string)')
	assert_composite_spelling(Type(Channel{ elem_type: &int_pointer, is_mut: true }), 'chan mut int')
	assert_composite_spelling(Type(Channel{ elem_type: &builtin_int_type, is_mut: true }), 'chan int')
	assert_composite_spelling(Type(Channel{ elem_type: &int_pointer }), 'chan &int')
}

fn test_composite_spelling_ids_do_not_cache_mutated_borrowed_function_parameters() {
	mut params := [FnParam{ typ: &builtin_int_type }, FnParam{ typ: &builtin_bool_type },
		FnParam{ typ: &builtin_string_type }]
	borrowed := Type(FnType{ params: params })
	mut interner := new_type_interner()
	_, canonical := interner.canonicalize(borrowed)
	assert_composite_spelling(borrowed, 'fn(int, bool, string)')
	assert_composite_spelling(*canonical, 'fn(int, bool, string)')
	params[1] = FnParam{ typ: &Type(u8_), is_mut: true }
	assert_composite_spelling(borrowed, 'fn(int, mut u8, string)')
	assert_composite_spelling(*canonical, 'fn(int, bool, string)')
	params[1] = FnParam{ typ: &builtin_bool_type }
	assert_composite_spelling(borrowed, 'fn(int, bool, string)')
}

fn test_composite_spelling_ids_do_not_cache_mutated_borrowed_multi_returns() {
	mut values := [Type(int_), Type(bool_), Type(string_)]
	borrowed := Type(MultiReturn{ types: values })
	mut interner := new_type_interner()
	_, canonical := interner.canonicalize(borrowed)
	assert_composite_spelling(borrowed, '(int, bool, string)')
	assert_composite_spelling(*canonical, '(int, bool, string)')
	values[1] = Type(u8_)
	assert_composite_spelling(borrowed, '(int, u8, string)')
	assert_composite_spelling(*canonical, '(int, bool, string)')
	values[1] = Type(bool_)
	assert_composite_spelling(borrowed, '(int, bool, string)')
}

fn test_composite_spelling_ids_preserve_actual_void_variant_checks() {
	nominal_void := Type(Struct{ name_id: flat.source_type_void })
	empty_nominal := Type(Struct{})
	assert_composite_spelling(Type(Pointer{ base_type: &builtin_void_type }), 'voidptr')
	assert_composite_spelling(Type(Pointer{ base_type: &nominal_void }), '&void')
	assert_composite_spelling(Type(FnType{}), 'fn()')
	assert_composite_spelling(Type(FnType{ return_type: &nominal_void }), 'fn() void')
	assert_composite_spelling(Type(FnType{ return_type: &empty_nominal }), 'fn() ')
	assert_composite_spelling(Type(MultiReturn{}), '()')
}

fn test_composite_fixed_array_lengths_keep_signed_and_wide_values() {
	assert_composite_spelling(Type(ArrayFixed{ elem_type: &builtin_int_type, len: -1 }), '[-1]int')
	if sizeof(int) == 8 {
		assert_composite_spelling(Type(ArrayFixed{ elem_type: &builtin_int_type, len: int(i64(4294967297)) }), '[4294967297]int')
	}
	assert flat.source_type_name(flat.source_type_fixed_array(-1, '', flat.source_type_int, true)) == 'int[-1]'
	assert flat.source_type_name(flat.source_type_fixed_array(0, ' n + 1 ', flat.source_type_int, true)) == 'int[ n + 1 ]'
}

fn test_composite_spelling_ids_preserve_large_borrowed_signatures() {
	mut params := []FnParam{len: 17, init: FnParam{ typ: &builtin_int_type }}
	function := Type(FnType{ params: params })
	assert_composite_spelling(function, 'fn(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int)')
	params[8] = FnParam{ typ: &builtin_string_type }
	assert_composite_spelling(function, 'fn(int, int, int, int, int, int, int, int, string, int, int, int, int, int, int, int, int)')
	mut values := []Type{len: 17, init: builtin_int_type}
	returns := Type(MultiReturn{ types: values })
	assert_composite_spelling(returns, '(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int)')
	values[8] = builtin_string_type
	assert_composite_spelling(returns, '(int, int, int, int, int, int, int, int, string, int, int, int, int, int, int, int, int)')
}
