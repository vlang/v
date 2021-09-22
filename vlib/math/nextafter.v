module math

// nextafter32 returns the next representable f32 value after x towards y.
//
// special cases are:
// nextafter32(x, x)   = x
// nextafter32(nan, y) = nan
// nextafter32(x, nan) = nan
pub fn nextafter32(x f32, y f32) f32 {
	mut r := f32(0.0)
	if is_nan(f64(x)) || is_nan(f64(y)) {
		r = f32(nan())
	} else if x == y {
		r = x
	} else if x == 0 {
		r = f32(copysign(f64(f32_from_bits(1)), f64(y)))
	} else if (y > x) == (x > 0) {
		r = f32_from_bits(f32_bits(x) + 1)
	} else {
		r = f32_from_bits(f32_bits(x) - 1)
	}
	return r
}

// nextafter returns the next representable f64 value after x towards y.
//
// special cases are:
// nextafter(x, x)   = x
// nextafter(nan, y) = nan
// nextafter(x, nan) = nan
pub fn nextafter(x f64, y f64) f64 {
	mut r := 0.0
	if is_nan(x) || is_nan(y) {
		r = nan()
	} else if x == y {
		r = x
	} else if x == 0 {
		r = copysign(f64_from_bits(1), y)
	} else if (y > x) == (x > 0) {
		r = f64_from_bits(f64_bits(x) + 1)
	} else {
		r = f64_from_bits(f64_bits(x) - 1)
	}
	return r
}
