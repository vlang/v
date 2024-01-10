fn test_conv_to_bool() {
	v := 0
	mut b := v != 0
	assert !b
	b = u64(&v) != 0
	assert b
	// check true -> 1
	assert int(b) == 1

	// branchless tests (can be important for manual optimization)
	arr := [7, 8]!
	e := arr[int(b)]
	assert e == 8
	b = e < 0
	assert arr[int(b)] == 7
}
