module math

// double_factorial returns n!!.
pub fn double_factorial(n int) i64 {
	if n < 0 {
		return 0
	}
	if n <= 1 {
		return 1
	}
	mut res := i64(1)
	mut i := n
	for i > 1 {
		res *= i64(i)
		i -= 2
	}
	return res
}
