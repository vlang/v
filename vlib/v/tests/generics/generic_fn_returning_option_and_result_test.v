fn get_value[T]() ?T {
	return none
}

fn get_value_result[T]() !T {
	return error('no result')
}

fn test_generic_function_that_returns_an_option() {
	value := get_value[&int]() or { &int(0) }
	assert value == unsafe { nil }
	sval := get_value[string]() or { 'abc' }
	assert sval == 'abc'
	uval := get_value[u64]() or { 123 }
	assert uval == 123
}

fn test_generic_function_that_returns_an_error() {
	sval := get_value_result[string]() or { 'xyz' }
	assert sval == 'xyz'
	ival := get_value_result[int]() or { 456 }
	assert ival == 456
	pval := get_value_result[&int]() or { &int(789) }
	assert u64(pval) == u64(&int(789))
}
