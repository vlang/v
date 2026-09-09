fn ret_int() fn (int) int {
	return fn (i int) int {
		return i
	}
}

fn test_fn_var_name_using_reserved() {
	new := ret_int()
	println(new(42))
	assert new(42) == 42
}

fn call_fn(do fn () int) int {
	return do()
}

// https://github.com/vlang/v/issues/27109
fn test_reserved_name_as_fn_param_and_call_arg() {
	do := fn () int {
		return -1
	}
	assert call_fn(do) == -1
}

fn call_result_fn(do fn () !int) !int {
	return do()!
}

fn test_reserved_name_as_result_fn_param_and_call_arg() {
	do := fn () !int {
		return -1
	}
	r := call_result_fn(do)!
	assert r == -1
}
