module math

// modinv returns the modular inverse of a mod m, or 0 if none.
pub fn modinv(a i64, m i64) i64 {
	g, x, _ := extended_gcd(a, m)
	if g != 1 {
		return 0
	}
	res := x % m
	if res < 0 {
		return res + m
	}
	return res
}
