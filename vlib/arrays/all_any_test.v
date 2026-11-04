module arrays

fn test_all_any() {
	assert all([2, 4, 6], fn (x int) bool { return x % 2 == 0 })
	assert !all([2, 3, 6], fn (x int) bool { return x % 2 == 0 })
	assert some([1, 3, 4], fn (x int) bool { return x % 2 == 0 })
	assert !some([1, 3, 5], fn (x int) bool { return x % 2 == 0 })
}
