fn option() ![]int {
	return []
}

fn func() {}

fn test_main() {
	for _ in option() or {
		func()
		return
	} {
	}
	assert true
}
