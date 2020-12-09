fn show_info(a string) string {
	return a
}

fn test_interpolation_string_args() {
	assert '${show_info("abc")}' == 'abc'
	assert '${show_info("bac")}' == 'bac'
	assert '${"aaa"}' == 'aaa'
}
