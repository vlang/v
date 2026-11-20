module math

// fibonacci returns the n-th Fibonacci number (0-indexed).
pub fn fibonacci(n int) i64 {
	if n <= 0 {
		return 0
	}
	if n == 1 {
		return 1
	}
	mut a := i64(0)
	mut b := i64(1)
	for _ in 2 .. n + 1 {
		c := a + b
		a = b
		b = c
	}
	return b
}
