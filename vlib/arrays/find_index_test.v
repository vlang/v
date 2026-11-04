module arrays

fn test_find_index() {
	assert find_index([1, 2, 3], fn (x int) bool { return x > 1 }) == 1
	assert find_index([1, 2, 3], fn (x int) bool { return x > 5 }) == -1
}
