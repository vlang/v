module math

// totient returns Euler's totient function φ(n).
pub fn totient(n i64) i64 {
	if n <= 0 {
		return 0
	}
	mut result := n
	mut x := n
	mut p := i64(2)
	for p * p <= x {
		if x % p == 0 {
			for x % p == 0 {
				x /= p
			}
			result -= result / p
		}
		p++
	}
	if x > 1 {
		result -= result / x
	}
	return result
}
