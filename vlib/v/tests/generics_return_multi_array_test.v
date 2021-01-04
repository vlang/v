fn example<T>(data []T) [][]T {
	return [data]
}

fn test_generic_return_multi_array() {
	data := [1, 2, 3]
	d2 := example(data)
	assert d2 == [[1, 2, 3]]
}
