struct Number[T] {
	value T
}

fn f[T](numbers &Number[T], idx int) T {
	return unsafe { numbers[idx].value }
}

fn test_indexing_a_pointer_to_generic_instances() {
	numbers := [10]Number[int]{init: Number[int]{
		value: index * 10
	}}
	assert f(&numbers[0], 3) == 30
	assert f(&numbers[0], 9) == 90
}
