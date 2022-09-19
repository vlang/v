fn example() ! {
	return error('oh no')
}

fn test_simple() {
	assert true
	assert true
}

fn test_example() ! {
	assert true
	assert true
	example() or { return error('failing test with return, err: $err') }
}

fn test_example_2() {
	assert true
	assert true
	example()?
}
