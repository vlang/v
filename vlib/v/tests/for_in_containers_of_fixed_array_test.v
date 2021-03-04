fn test_for_in_containers_of_fixed_array() {
	mut rets := []string{}
	arr := [][2]int{len: 3}

	for pair in arr {
		println(pair)
		rets << '$pair'
	}
	assert rets[0] == '[0, 0]'
	assert rets[1] == '[0, 0]'
	assert rets[2] == '[0, 0]'
}
