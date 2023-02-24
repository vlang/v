fn f_test(args ?[2]int) ? {
	println(args)
	assert args?.len == 2
}

fn test_simple() {
	mut maybe_fixed1 := ?[3]int(none)
	println(maybe_fixed1) // Option(error: none)
}

fn test_simple_assign() {
	mut maybe_fixed1 := ?[3]int(none)
	assert maybe_fixed1 == none

	maybe_fixed1 = [1, 2, 3]!
	assert maybe_fixed1 != none

	println(maybe_fixed1) // Option([1, 2, 3])
}

fn test_array_fixed_param() {
	f_test([1, 2]!)
}
