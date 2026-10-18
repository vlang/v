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
	counter.log.clear()
	assert group_take(group_fixed_arg(mut counter), ...group_spread_arg(mut counter)) == 1
	assert counter.log == ['fixed', 'spread']
	counter.log.clear()
	assert '${group_take(group_fixed_arg(mut counter), ...group_spread_arg(mut counter))}' == '1'
	assert counter.log == ['fixed', 'spread']
}

fn test_nested_arrays_are_single_variadic_elements() {
	inputs := [[1, 2], [3, 4]]
	assert arrays.group(inputs) == [[[1, 2]], [[3, 4]]]
	assert '${arrays.group(inputs)}' == '[[[1, 2]], [[3, 4]]]'
	assert '${arrays.group([[1, 2], [3, 4]])}' == '[[[1, 2]], [[3, 4]]]'
	assert arrays.group(inputs, inputs) == [[[1, 2], [1, 2]], [[3, 4], [3, 4]]]
	assert '${arrays.group(inputs, inputs)}' == '[[[1, 2], [1, 2]], [[3, 4], [3, 4]]]'
	cubes := [[[1, 2]], [[3, 4]]]
	assert arrays.group(cubes) == [[[[1, 2]]], [[[3, 4]]]]
	assert '${arrays.group(cubes)}' == '[[[[1, 2]]], [[[3, 4]]]]'
	assert arrays.group(...cubes) == [[[1, 2], [3, 4]]]
	assert '${arrays.group(...cubes)}' == '[[[1, 2], [3, 4]]]'
}

const group_const_inputs = [[1, 2], [3, 4]]
const group_const_single_text = '${arrays.group(group_const_inputs)}'
const group_const_spread_text = '${arrays.group(...group_const_inputs)}'

fn test_const_interpolation_preserves_variadic_packing() {
	assert group_const_single_text == '[[[1, 2]], [[3, 4]]]'
	assert group_const_spread_text == '[[1, 3], [2, 4]]'
}

fn group_in_generic_body[T](input []T) [][]T {
	return arrays.group(input)
}

fn group_spread_in_generic_body[T](inputs ...[]T) [][]T {
	return arrays.group(...inputs)
}

fn test_nested_generic_body_variadic_inference() {
	inputs := [[1, 2], [3, 4]]
	assert group_in_generic_body(inputs) == [[[1, 2]], [[3, 4]]]
	assert '${group_in_generic_body(inputs)}' == '[[[1, 2]], [[3, 4]]]'
	assert group_spread_in_generic_body(inputs) == [[[1, 2]], [[3, 4]]]
	assert '${group_spread_in_generic_body(inputs)}' == '[[[1, 2]], [[3, 4]]]'
	assert group_spread_in_generic_body(...inputs) == [[1, 3], [2, 4]]
	assert '${group_spread_in_generic_body(...inputs)}' == '[[1, 3], [2, 4]]'
}

fn group_collect[T](values ...T) []T {
	return values
}

fn test_single_array_against_a_direct_variadic_placeholder() {
	assert group_collect([1, 2]) == [[1, 2]]
	assert '${group_collect([1, 2])}' == '[[1, 2]]'
	assert group_collect([[1, 2]]) == [[[1, 2]]]
	assert '${group_collect([[1, 2]])}' == '[[[1, 2]]]'
	assert group_collect(...[[1, 2], [3, 4]]) == [[1, 2], [3, 4]]
	assert '${group_collect(...[[1, 2], [3, 4]])}' == '[[1, 2], [3, 4]]'
}
