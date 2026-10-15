import arrays

struct GroupInputCounter {
mut:
	calls int
}

fn group_inputs(mut counter GroupInputCounter) [][]int {
	counter.calls++
	return [[1, 2], [3, 4]]
}

fn test_inferred_variadic_array_group_preserves_zip_semantics() {
	grouped := arrays.group([1, 2])
	assert grouped == [[1], [2]]
	assert '${arrays.group([1, 2])}' == '[[1], [2]]'
	assert arrays.group([1, 2], [3, 4]) == [[1, 3], [2, 4]]
	assert '${arrays.group([1, 2], [3, 4])}' == '[[1, 3], [2, 4]]'
	inputs := [[1, 2], [3, 4]]
	assert arrays.group(...inputs) == [[1, 3], [2, 4]]
	assert '${arrays.group(...inputs)}' == '[[1, 3], [2, 4]]'
	mut counter := GroupInputCounter{}
	assert '${arrays.group(...group_inputs(mut counter))}' == '[[1, 3], [2, 4]]'
	assert counter.calls == 1
}
