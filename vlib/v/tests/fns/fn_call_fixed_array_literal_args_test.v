fn test_fn_call_fixed_array_literal_args() {
	ret := get_str([1]!)
	assert ret == '[1]'
}

fn get_str(t [1]int) string {
	println(t)
	return '${t}'
}
