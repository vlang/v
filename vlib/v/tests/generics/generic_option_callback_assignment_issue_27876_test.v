fn run_option_callback[T](callback fn () T) ?T {
	mut result := T{}
	result = callback()
	return result
}

fn plain_int_callback() int {
	return 42
}

fn option_int_callback() ?int {
	return 42
}

fn test_generic_callback_assignment_uses_current_instantiation_types() {
	assert run_option_callback(plain_int_callback)? == 42
	assert run_option_callback(option_int_callback)? == 42
}
