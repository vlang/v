fn get_int() int {
	return 42
}

fn get_option() ?int {
	return 42
}

fn get_result() !int {
	return 42
}

fn run_identity[T](cb fn () T) T {
	return cb()
}

fn run_option_ret[T](cb fn () T) ?T {
	mut r := T{}
	r = cb()
	return r
}

fn run_result_ret[T](cb fn () T) !T {
	mut r := T{}
	r = cb()
	return r
}

fn run_option_cb_option_ret[T](cb fn () ?T) ?T {
	mut r := T{}
	r = cb()?
	return r
}

fn run_result_cb_option_ret[T](cb fn () !T) ?T {
	mut r := T{}
	r = cb() or { panic(err) }
	return r
}

fn run_result_cb_result_ret[T](cb fn () !T) !T {
	mut r := T{}
	r = cb()!
	return r
}

fn test_generic_fn_plain_callback_return_infers_plain_type() {
	assert run_identity(get_int) == 42
}

fn test_generic_fn_plain_callback_return_infers_option_type() {
	result := run_identity(get_option)
	assert result? == 42
}

fn test_generic_fn_option_callback_return_infers_plain_type() {
	result := run_option_ret(get_int)
	assert result? == 42
}

// TODO: uncomment when issue is fixed (cf. #27876)
// fn test_generic_fn_option_callback_return_infers_option_type() {
// 	result := run_option_ret(get_option)
// 	assert result? == 42
// }

fn test_generic_fn_result_callback_result_return() {
	assert run_result_ret(get_int)! == 42
}

fn test_generic_fn_option_callback_option_return() {
	result := run_option_cb_option_ret(get_option)
	assert result? == 42
}

fn test_generic_fn_result_callback_option_return() {
	result := run_result_cb_option_ret(get_result)
	assert result? == 42
}

fn test_generic_fn_callback_result_return() {
	assert run_result_cb_result_ret(get_result)! == 42
}
