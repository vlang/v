// For issue 27786: the temp var of an `if x := opt {` guard inside a generic
// function was declared with the option type of the last checked
// instantiation, producing invalid C code for all other instantiations.
fn unwrap_option_or[T](option_type ?T, default_value T) T {
	if some_value := option_type {
		return some_value
	}
	return default_value
}

fn unwrap_option_or_option[T](option_type ?T, default_option ?T) ?T {
	if some_value := option_type {
		return some_value
	}
	return default_option
}

fn test_if_guard_on_generic_option_param() {
	assert unwrap_option_or[string](?string('hi'), 'def') == 'hi'
	assert unwrap_option_or[string](none, 'def') == 'def'
	assert unwrap_option_or[bool](?bool(true), false) == true
	assert unwrap_option_or[i32](?i32(7), 3) == 7
	a := unwrap_option_or_option[string](none, ?string('fallback'))
	assert a? == 'fallback'
	b := unwrap_option_or_option[i32](?i32(5), none)
	assert b? == 5
}
