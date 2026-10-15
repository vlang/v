import arrays

struct GroupInputCounter {
mut:
	calls int
}

struct GroupOrderCounter {
mut:
	log []string
}

fn group_fixed_arg(mut counter GroupOrderCounter) int {
	counter.log << 'fixed'
	return 0
}

fn group_spread_arg(mut counter GroupOrderCounter) [][]int {
	counter.log << 'spread'
	return [[1]]
}

fn group_take[T](value int, inputs ...[]T) int {
	return value + inputs.len
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

fn test_spread_tail_preserves_fixed_argument_evaluation_order() {
	mut counter := GroupOrderCounter{}
	assert group_take[int](group_fixed_arg(mut counter), ...group_spread_arg(mut counter)) == 1
	assert counter.log == ['fixed', 'spread']
	counter.log.clear()
	assert '${group_take[int](group_fixed_arg(mut counter), ...group_spread_arg(mut counter))}' == '1'
	assert counter.log == ['fixed', 'spread']
}
