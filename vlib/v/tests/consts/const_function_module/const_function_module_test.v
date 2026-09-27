import answer

fn test_module_constant_and_function_have_distinct_c_symbols() {
	assert answer.value == 42
	assert answer.value() == 42
	assert answer.cached_value() == 42
}
