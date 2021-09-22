fn include<T>(a T, b []T) bool {
	return a in b
}

fn test_generics_multi_array_in() {
	println(include(1, [1]))
	assert include(1, [1])

	println(include([1], [[1]]))
	assert include([1], [[1]])

	println(include([[1]], [[[1]]]))
	assert include([[1]], [[[1]]])
}
