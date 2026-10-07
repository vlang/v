module types

import v.flat

fn test_variadic_function_type_metadata_survives_parsing_and_copying() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	variadic := tc.parse_type('fn (int, ...string) int')
	fixed := tc.parse_type('fn (int, []string) int')
	assert variadic is FnType
	assert fixed is FnType
	variadic_fn := variadic as FnType
	fixed_fn := fixed as FnType
	assert variadic_fn.is_variadic
	assert !fixed_fn.is_variadic
	assert variadic.name() == 'fn(int, ...string) int'
	assert fixed.name() == 'fn(int, []string) int'
	assert !semantic_types_equal(variadic, fixed)
	assert semantic_type_hash(variadic) != semantic_type_hash(fixed)
	variadic_id, _ := tc.intern_type(variadic)
	fixed_id, _ := tc.intern_type(fixed)
	assert variadic_id != fixed_id
	assert tc.c_type(variadic) == tc.c_type(fixed)
	cloned := clone_owned_type(variadic)
	assert cloned is FnType
	assert (cloned as FnType).is_variadic
	assert semantic_types_equal(cloned, variadic)
	roundtrip := tc.parse_type(variadic.name())
	assert semantic_types_equal(roundtrip, variadic)
}

fn test_variadic_function_type_metadata_survives_generic_substitution() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	generic := tc.parse_type('fn (T, ...T) T')
	text_substitution := tc.substitute_generic_type(generic, ['string'], ['T'])
	value_substitution := tc.substitute_generic_type_values(generic, [Type(string_)], ['T'])
	for specialized in [text_substitution, value_substitution] {
		assert specialized is FnType
		assert (specialized as FnType).is_variadic
		assert specialized.name() == 'fn(string, ...string) string'
	}
}

fn test_named_function_value_type_keeps_variadic_signature() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.fn_param_types['callback'] = [Type(int_), Type(Array{ elem_type: Type(string_) })]
	tc.fn_ret_types['callback'] = Type(int_)
	tc.fn_variadic['callback'] = true
	fn_type := tc.fn_type_from_key('callback') or { panic('missing callback type') }
	assert fn_type is FnType
	assert (fn_type as FnType).is_variadic
}
