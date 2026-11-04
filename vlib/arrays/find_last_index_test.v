module arrays

fn test_find_last_index() {
	assert find_last_index([1, 2, 3, 2], fn (x int) bool { return x == 2 }) == 3
}
