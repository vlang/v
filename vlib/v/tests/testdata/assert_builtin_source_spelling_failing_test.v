struct AssertSourceType {
	value int
}

fn test_assert_sizeof_literal() {
	assert 'sizeof(AssertSourceType)' == 'sizeof(int)'
}

fn test_assert_offsetof_literal() {
	assert '__offsetof(AssertSourceType, value)' == 'offsetof(AssertSourceType, value)'
}

fn test_assert_pointer_sizeof() {
	assert sizeof(&AssertSourceType) == 0
}

fn test_assert_fixed_array_sizeof() {
	assert sizeof([2]AssertSourceType) == 0
}
