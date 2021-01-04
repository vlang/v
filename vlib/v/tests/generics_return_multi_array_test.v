fn example<T>(data []T) [][]T {
	return [data]
}

fn test_generic_return_multi_array() {
	d1 := [1, 2, 3]
	d2 := example(d1)
	assert d2 == [[1, 2, 3]]

	d3 := [1.1, 2.2, 3.3]
	d4 := example(d3)
	assert d4 == [[1.1, 2.2, 3.3]]
}
