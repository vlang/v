fn byte(value int) int {
	return value + 1
}

fn test_byte_function_with_argument_is_not_a_cast() {
	assert byte(8) == 9
	assert byte(255) == 256
}
