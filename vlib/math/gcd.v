module math

// gcd_many returns the gcd of all values (0 for an empty input).
pub fn gcd_many[T](values []T) T {
	mut g := u64(0)
	for v in values {
		g = gcd_u64(g, u64mag(v))
	}
	if g > u64(maxof[T]()) {
		return maxof[T]()
	}
	return T(g)
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
	$if T is int || T is i8 || T is i16 || T is i32 || T is i64 || T is u8 || T is u16 || T is u32 || T is u64 {
	} $else {
		$compile_error('math: gcd and lcm support integer widths up to 64 bits')
	}
	if v < T(0) {
		return ~u64(v) + 1
	}
	return u64(v)
}

fn gcd_u64(a u64, b u64) u64 {
	mut x := a
	mut y := b
	for y != 0 {
		x, y = y, x % y
	}
	return x
}
