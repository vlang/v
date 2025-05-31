fn test_array_of_fixed_array() {
	mut arr := [][3]int{}

	fixed_arr1 := [3]int{}
	arr << fixed_arr1

	fixed_arr2 := [1, 2, 3]!
	arr << fixed_arr2

	println(arr)

	assert arr.len == 2
	assert '${arr[0]}' == '[0, 0, 0]'
	assert '${arr[1]}' == '[1, 2, 3]'
}
