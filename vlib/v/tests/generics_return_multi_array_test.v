fn example1<T>(data []T) [][]T {
	return [data]
}

fn example2<T>(data [][]T) [][][]T {
	return [data]
}

fn example3<T>(data [][][]T) [][][][]T {
	return [data]
}

fn test_generic_return_multi_array() {
	d1 := [1, 2, 3]
	d2 := example1(d1)
	assert d2 == [[1, 2, 3]]

	d11 := [[1, 2, 3]]
	d22 := example2(d11)
	assert d22 == [[[1, 2, 3]]]

	d111 := [[[1, 2, 3]]]
	d222 := example3(d111)
	assert d222 == [[[[1, 2, 3]]]]
}
