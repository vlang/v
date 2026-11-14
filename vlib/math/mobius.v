module math

// mobius returns the Möbius function μ(n).
pub fn mobius(n i64) i64 {
	if n <= 0 {
		return 0
	}
	if n == 1 {
		return 1
	}
	factors := prime_factors(n)
	// check square factor
	for i in 0 .. factors.len - 1 {
		if factors[i] == factors[i + 1] {
			return 0
		}
	}
	if factors.len % 2 == 0 {
		return 1
	}
	return -1
}
