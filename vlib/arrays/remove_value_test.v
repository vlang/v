module arrays

fn test_remove_value() {
	assert remove_value([1, 2, 1, 3], 1) == [2, 3]
}
