fn test_assert_preserves_wide_integer_constant_expression() {
	value := i64(-9223372036854775807 - 1)
	assert value == -9223372036854775807 - 1
}
