const answer = answer()
const generic_answer = generic_answer[int]()

fn answer() int {
	return 42
}

fn generic_answer[T]() T {
	return T(43)
}

fn test_const_initializer_can_call_a_function_with_the_same_name() {
	assert answer == 42
	assert answer() == 42
	assert generic_answer == 43
	assert generic_answer[int]() == 43
}
