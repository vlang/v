fn test_array_2d_append_empty_array() {
	mut b := [][]int{}
	b << [1, 2, 3]
	b << []
	assert b[0][0] == 1
	assert b[1] == []
}
