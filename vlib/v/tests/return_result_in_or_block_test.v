fn unwrap_int() ?int {
	return 1
}

fn unwrap_function1() !int {
	return unwrap_int() or { error('we are issing the return?') }
}

fn unwrap_function2() ?int {
	return unwrap_int() or { none }
}

fn test_return_result_in_or_block() {
	x1 := unwrap_function1() or { panic(err) }
	println(x1)
	assert x1 == 1

	x2 := unwrap_function2() or { panic(err) }
	println(x2)
	assert x2 == 1
}
