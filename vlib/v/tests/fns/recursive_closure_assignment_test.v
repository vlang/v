fn test_recursive_closure_assignment() {
	one := 1
	mut fact := fn (n int) int {
		return 1
	}
	fact = fn [one, fact] (n int) int {
		if n <= 1 {
			return one
		}
		return n * fact(n - 1)
	}
	assert fact(5) == 120
}
