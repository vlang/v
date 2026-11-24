import array_scalar_push_collision

fn test_user_function_named_array_push_remains_an_ordinary_call() {
	array_scalar_push_collision.check()
}

// An append outside the module is lowered to the runtime helper `array_push`; the
// imported module's function of that name must not take the call.
fn test_appends_outside_the_module_use_the_runtime_helper() {
	mut values := []int{}
	values << 5
	values << [6, 7]
	assert values == [5, 6, 7]
	mut floats := []f32{}
	floats << f32(1.5)
	assert floats == [f32(1.5)]
}
