fn test_doubler() {
	make_doubler := fn() fn(int) int {
		return fn(n int) int {
			return n + n
		}
	}
	doubler := make_doubler()
	assert doubler(10) == 20
}
