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
	// (x + 1) / 2 without overflowing at max i64.
	mut y := x / 2 + x % 2
	for y < x {
		x = y
		// (x + n / x) / 2 without overflowing: both terms are
		// non-negative i64, so their sum always fits u64.
		y = i64((u64(x) + u64(n / x)) / 2)
	}
	return x
}
