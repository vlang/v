const answer = answer()

fn answer() int {
	return 42
}

fn test_const_initializer_can_call_a_function_with_the_same_name() {
	assert answer == 42
	assert answer() == 42
}
