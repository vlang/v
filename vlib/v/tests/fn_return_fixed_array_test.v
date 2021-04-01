fn test_fn_return_fixed_array() {
	x := get3()
	dump(x)
	assert '$x' == '[1, 2, 3]'
}

fn get3() [3]int {
	mut res := [3]int{}
	res[0] = 1
	res[1] = 2
	res[2] = 3
	return res
}
