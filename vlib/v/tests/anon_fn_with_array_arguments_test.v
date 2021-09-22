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
