fn test_skip_check_sort_arg_a_b() {
	arr := [4, 2, 1, 3]
	a := arr.sorted(a < b)
	assert a == [1, 2, 3, 4]
	b := arr.sorted(a > b)
	assert b == [4, 3, 2, 1]
}
