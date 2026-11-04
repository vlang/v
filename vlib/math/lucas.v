module math

// lucas returns the n-th Lucas number.
pub fn lucas(n int) i64 {
	if n == 0 {
		return 2
	}
	if n == 1 {
		return 1
	}
	mut a := i64(2)
	mut b := i64(1)
	for _ in 2 .. n + 1 {
		c := a + b
		a = b
		b = c
	}
	return b
}
