fn test_for_in_mut_array_index() {
	mut arr := [1, 2, 3, 4, 5]
	mut rets := []int{}

	for mut val in arr {
		inx := arr.index(val)
		println(inx)
		rets << inx
	}

	assert rets == [0, 1, 2, 3, 4]
}
