module math

// gcd_many returns the gcd of all values (0 for an empty input).
pub fn gcd_many[T](values []T) T {
	mut g := T(0)
	for v in values {
		g = gcd(g, v)
	}
	return g
}

// lcm_many returns the lcm of all values (1 for an empty input).
pub fn lcm_many[T](values []T) T {
	mut l := T(1)
	for v in values {
		l = lcm(l, v)
	}
	return l
}

// u64mag returns the magnitude of v as u64. Unlike a negation it cannot
// overflow: min maps to max+1 exactly (two's complement in the u64 domain).
fn u64mag[T](v T) u64 {
	if v < T(0) {
		return ~u64(v) + 1
	}
	return u64(v)
}
