module math

// prime_factors returns the prime factorization of n.
pub fn prime_factors(n i64) []i64 {
	mut factors := []i64{}
	mut x := n
	if x < 2 {
		return factors
	}
	mut d := i64(2)
	for d * d <= x {
		for x % d == 0 {
			factors << d
			x /= d
		}
		d++
	}
	if x > 1 {
		factors << x
	}
	return factors
}
