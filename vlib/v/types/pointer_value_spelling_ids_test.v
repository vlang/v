module types

import v.flat

// Keep the old textual rule as an oracle for spelling collisions across type variants.
fn pointer_value_legacy_spelling_compatible(tc &TypeChecker, actual Type, expected Type) bool {
	if actual is Pointer {
		base := actual.base_type
		clean := if base is Alias { *base.base_type } else { *base }
		if clean is Void || clean is Unknown || clean is None {
			return false
		}
		clean_base := unalias_type(base)
		clean_expected := unalias_type(expected)
		primitive_mismatch := clean_base is Primitive && clean_expected is Primitive
			&& clean_base.name() != clean_expected.name()
		if !primitive_mismatch && tc.type_compatible(base, expected) {
			return true
		}
		actual_name := actual.name()
		expected_name := expected.name()
		return (actual_name.starts_with('&') && actual_name[1..] == expected_name)
			|| base.name() == expected_name
	}
	actual_name := actual.name()
	return actual_name.starts_with('&') && actual_name[1..] == expected.name()
}

fn pointer_value_test_nominals(name_id u32) []Type {
	return [Type(Struct{ name_id: name_id }), Type(Enum{ name_id: name_id }),
		Type(Interface{ name_id: name_id }), Type(SumType{ name_id: name_id }),
		Type(Alias{ name_id: name_id, base_type: builtin_int_type })]
}

fn test_pointer_value_spelling_ids_keep_nominal_prefix_and_empty_spelling_rules() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	for spellings in [['&Item', 'Item'], ['&&Item', '&Item'], ['& Item', ' Item'], ['&Item ', 'Item '],
		['&', '']] {
		for actual in pointer_value_test_nominals(flat.source_type_id(spellings[0])) {
			for expected in pointer_value_test_nominals(flat.source_type_id(spellings[1])) {
				assert tc.pointer_value_compatible(actual, expected)
				assert tc.pointer_value_compatible(actual, expected) ==
					pointer_value_legacy_spelling_compatible(&tc, actual, expected)
			}
		}
	}
	for spellings in [['Item', 'Item'], ['', ''], ['&&Item', 'Item'], ['&Item ', 'Item'],
		[' &Item', '&Item']] {
		for actual in pointer_value_test_nominals(flat.source_type_id(spellings[0])) {
			expected := Type(Struct{ name_id: flat.source_type_id(spellings[1]) })
			assert !tc.pointer_value_compatible(actual, expected)
			assert tc.pointer_value_compatible(actual, expected) ==
				pointer_value_legacy_spelling_compatible(&tc, actual, expected)
		}
	}
	// A nominal alias uses its own spelling here, rather than its pointed-to payload.
	alias := Type(Alias{ name_id: flat.source_type_id('&int'), base_type: builtin_string_type })
	assert tc.pointer_value_compatible(alias, builtin_int_type)
	assert !tc.pointer_value_compatible(alias, builtin_string_type)
}

fn test_pointer_value_spelling_ids_keep_primitive_pointer_names() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	for actual in [builtin_int_type, builtin_charptr_type, builtin_byteptr_type, builtin_voidptr_type,
		Type(Pointer{ base_type: builtin_int_type }), Type(Pointer{ base_type: builtin_i64_type }),
		Type(Pointer{ base_type: new_type_reference(Type(Primitive{})) }),
		Type(Pointer{ base_type: new_type_reference(Type(Primitive{ props: .unsigned, size: 16 })) }),
		Type(Pointer{ base_type: new_type_reference(Type(Primitive{ props: .integer, size: 24 })) })] {
		for expected in [builtin_int_type, builtin_i64_type, builtin_u8_type, builtin_char_type,
			builtin_void_type, Type(Primitive{}), Type(Primitive{ props: .unsigned, size: 16 }),
			Type(Primitive{ props: .integer, size: 24 })] {
			assert tc.pointer_value_compatible(actual, expected) ==
				pointer_value_legacy_spelling_compatible(&tc, actual, expected)
		}
	}
	assert tc.pointer_value_compatible(builtin_charptr_type, builtin_char_type)
	assert tc.pointer_value_compatible(builtin_byteptr_type, builtin_u8_type)
	assert !tc.pointer_value_compatible(builtin_voidptr_type, builtin_void_type)
	assert !tc.pointer_value_compatible(Type(Pointer{ base_type: builtin_int_type }), builtin_i64_type)
}

fn test_pointer_value_spelling_ids_keep_base_exclusions_and_single_alias_unwrap() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	for base in [builtin_void_type, Type(Unknown{}), builtin_none_type] {
		alias := Type(Alias{ name_id: flat.source_type_id('&Excluded'), base_type: new_type_reference(base) })
		for excluded in [base, alias] {
			actual := Type(Pointer{ base_type: new_type_reference(excluded) })
			for expected in [base, excluded, builtin_int_type,
				Type(Struct{ name_id: flat.source_type_id('&Excluded') })] {
				assert !tc.pointer_value_compatible(actual, expected)
				assert tc.pointer_value_compatible(actual, expected) ==
					pointer_value_legacy_spelling_compatible(&tc, actual, expected)
			}
		}
		// The historical exclusion unwraps exactly one alias, so a nested alias survives.
		outer := Type(Alias{ name_id: flat.source_type_id('Outer'), base_type: new_type_reference(alias) })
		actual := Type(Pointer{ base_type: new_type_reference(outer) })
		assert tc.pointer_value_compatible(actual, outer)
		assert tc.pointer_value_compatible(actual, outer) ==
			pointer_value_legacy_spelling_compatible(&tc, actual, outer)
	}
}

fn test_pointer_value_spelling_ids_keep_composite_and_nominal_spelling_collisions() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	values := [Type(Pointer{ base_type: builtin_void_type }),
		Type(Pointer{ base_type: builtin_int_type }), Type(Array{ elem_type: builtin_int_type }),
		Type(ArrayFixed{ elem_type: builtin_int_type, len: 2 }),
		Type(OptionType{ base_type: builtin_int_type }),
		Type(ResultType{ base_type: builtin_int_type }),
		Type(Map{ key_type: builtin_int_type, value_type: builtin_string_type }),
		Type(Channel{ elem_type: builtin_int_type }),
		Type(FnType{ params: [FnParam{ typ: builtin_int_type }], return_type: builtin_string_type }),
		Type(MultiReturn{ types: [builtin_int_type, builtin_string_type] })]
	for expected in values {
		for actual in pointer_value_test_nominals(flat.source_type_id('&${expected.name()}')) {
			assert tc.pointer_value_compatible(actual, expected)
			assert tc.pointer_value_compatible(actual, expected) ==
				pointer_value_legacy_spelling_compatible(&tc, actual, expected)
		}
		actual := Type(Pointer{ base_type: new_type_reference(expected) })
		assert tc.pointer_value_compatible(actual, expected)
		assert tc.pointer_value_compatible(actual, expected) ==
			pointer_value_legacy_spelling_compatible(&tc, actual, expected)
		// A composite spelling can also be stored by an otherwise unrelated nominal type.
		nominal_expected := Type(Struct{ name_id: expected.source_type_id() })
		assert tc.pointer_value_compatible(actual, nominal_expected)
		assert tc.pointer_value_compatible(actual, nominal_expected) ==
			pointer_value_legacy_spelling_compatible(&tc, actual, nominal_expected)
		if expected !is Pointer {
			assert !tc.pointer_value_compatible(expected, nominal_expected)
			assert tc.pointer_value_compatible(expected, nominal_expected) ==
				pointer_value_legacy_spelling_compatible(&tc, expected, nominal_expected)
		}
	}
}
