fn compare_int_arrays(mut left []int, right []int) bool {
	return left == right
}

fn test_mut_array_arg_comparison_still_works() {
	mut left := [1, 2]
	assert compare_int_arrays(mut left, [1, 2])
}
