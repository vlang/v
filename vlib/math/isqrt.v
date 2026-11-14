module math

// isqrt returns the integer square root of n (floor).
pub fn isqrt(n i64) i64 {
	if n < 0 {
		return 0
	}
	if n == 0 {
		return 0
	}
	mut x := n
	mut y := (x + 1) / 2
	for y < x {
		x = y
		y = (x + n / x) / 2
	}
	return x
}
