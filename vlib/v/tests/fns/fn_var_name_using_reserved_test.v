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
