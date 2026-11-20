module math

// is_prime reports whether n is prime.
pub fn is_prime(n i64) bool {
	if n < 2 {
		return false
	}
	if n < 4 {
		return true
	}
	if n % 2 == 0 {
		return false
	}
	mut i := i64(3)
	for i * i <= n {
		if n % i == 0 {
			return false
		}
		i += 2
	}
	return true
}
