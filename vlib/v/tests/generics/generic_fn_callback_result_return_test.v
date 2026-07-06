fn run[T](cb fn () !T) !T {
	mut r := T{}
	r = cb()!
	return r
}

fn test_generic_fn_callback_result_return() {
	cb := fn () !int {
		return 42
	}

	assert run(cb)! == 42
}
