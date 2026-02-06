fn count_str(array []string) int {
	return array.len
}

fn count_int(array []int) int {
	return array.len
}

fn count[T](array []T) int {
	return array.len
}

fn test_generics_with_empty_array_arg() {
	assert count_str(['one', 'two']) == 2
	assert count_str([]string{}) == 0
	assert count_str([]) == 0

	assert count_int([1, 2]) == 2
	assert count_int([]int{}) == 0
	assert count_int([]) == 0

	assert count[f64]([1.0, 2.0]) == 2
	assert count[f64]([]f64{}) == 0
	assert count[f64]([]) == 0

	assert count[int]([1, 2]) == 2
	assert count[int]([]int{}) == 0
	assert count[int]([]) == 0
}
