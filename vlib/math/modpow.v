module math

// modpow computes (base^exp) mod mod.
pub fn modpow(base i64, exp i64, mod i64) i64 {
	if mod == 1 {
		return 0
	}
	mut result := i64(1)
	mut b := base % mod
	mut exponent := exp
	for exponent > 0 {
		if exponent % 2 == 1 {
			result = (result * b) % mod
		}
		exponent /= 2
		b = (b * b) % mod
	}
	return result
}
