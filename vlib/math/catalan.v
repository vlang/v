module math

// catalan returns the n-th Catalan number.
pub fn catalan(n int) i64 {
	if n < 0 {
		return 0
	}
	return comb(2 * n, n) / i64(n + 1)
}
