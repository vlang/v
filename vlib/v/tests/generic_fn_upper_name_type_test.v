struct XX {
	x int
}

struct YY {
	y int
}

fn show_result<T, U>(x T, y U) bool {
	return true
}

fn test_generic_fn_upper_name_type() {
	assert show_result<int, bool>(1, false)
	assert show_result<string, XX>('s', XX{})
	assert show_result<XX, string>(XX{}, 's')
	assert show_result<XX, YY>(XX{}, YY{})
}
