fn test_fixed_array_literal_index() {
	println([1]int{}[0])
	assert [1]int{}[0] == 0

	println([1, 2]![1])
	assert [1, 2]![1] == 2
}
