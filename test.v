fn func_selector(n int) fn() int {
	return fn(n int) int {
		return n + n
	}
}
