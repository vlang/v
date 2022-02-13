struct Test {
	fn_test fn (x int, y [10]int, z []int) bool = fixed_array_fn
}

fn fixed_array_fn(x int, y [10]int, z []int) bool {
	return true
}

fn test_anon_fn_with_fixed_array_arguments() {
	assert true
}

fn fn_arg(f fn ([]int) int) int {
	return f([1, 2, 3])
}

fn test_anon_fn_with_array_arguments() {
	anon := fn (i []int) int {
		return 0
	}

	println(fn_arg(anon))
	assert fn_arg(anon) == 0
}
