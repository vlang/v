module arrays

fn test_find() {
	v := find([1, 2, 3], fn (x int) bool { return x > 1 })
	assert v or { 0 } == 2
}
