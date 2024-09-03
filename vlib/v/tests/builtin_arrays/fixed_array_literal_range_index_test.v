fn test_fixed_array_literal_range_index() {
	arr := [1, 2, 3, 4]![..]
	println(arr)
	println(typeof(arr).name)
	assert typeof(arr).name == '[]int'
	assert '${arr}' == '[1, 2, 3, 4]'
}
