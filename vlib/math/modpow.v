module math

// modpow computes (base^exp) mod mod.
pub fn modpow(base i64, exp i64, mod i64) i64 {
	if mod == 1 {
		return 0
	}
	if mod <= 0 {
		return 0
	}
	mut result := i64(1) % mod
	// Normalize base into [0, mod) without overflow.
	mut b := base % mod
	if b < 0 {
		b += mod
	}
	mut exponent := exp
	for exponent > 0 {
		if exponent % 2 == 1 {
			result = mod_mul(result, b, mod)
		}
		exponent /= 2
		b = mod_mul(b, b, mod)
	}
	return result
}

// mod_add returns (a + b) % mod for a, b in [0, mod) without overflow.
fn mod_add(a i64, b i64, mod i64) i64 {
	if a >= mod - b {
		return a - (mod - b)
	}
	return a + b
}

// mod_mul returns (a * b) % mod for a, b in [0, mod) without overflow,
// using double-and-add so no intermediate product exceeds 2*mod.
fn mod_mul(a i64, b i64, mod i64) i64 {
	mut res := i64(0)
	mut x := a
	mut y := b
	for y > 0 {
		if y & 1 == 1 {
			res = mod_add(res, x, mod)
		}
		x = mod_add(x, x, mod)
		y >>= 1
	}
	return res
}
