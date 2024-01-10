module math

// special cases are:
// sqrt(+inf) = +inf
// sqrt(±0) = ±0
// sqrt(x < 0) = nan
// sqrt(nan) = nan
@[inline]
pub fn sqrt(a f64) f64 {
	mut x := a
	if x == 0.0 || is_nan(x) || is_inf(x, 1) {
		return x
	}
	if x < 0.0 {
		return nan()
	}
	z, ex := frexp(x)
	w := x
	// approximate square root of number between 0.5 and 1
	// relative error of approximation = 7.47e-3
	x = 4.173075996388649989089e-1 + 5.9016206709064458299663e-1 * z // adjust for odd powers of 2
	if (ex & 1) != 0 {
		x *= sqrt2
	}
	x = ldexp(x, ex >> 1)
	// newton iterations
	x = 0.5 * (x + w / x)
	x = 0.5 * (x + w / x)
	x = 0.5 * (x + w / x)
	return x
}

// sqrtf calculates square-root of the provided value. (float32)
@[inline]
pub fn sqrtf(a f32) f32 {
	return f32(sqrt(a))
}

// sqrti calculates the integer square-root of the provided value. (i64)
pub fn sqrti(a i64) i64 {
	mut x := a
	mut q, mut r := i64(1), i64(0)
	for ; q <= x; {
		q <<= 2
	}
	for ; q > 1; {
		q >>= 2
		t := x - r - q
		r >>= 1
		if t >= 0 {
			x = t
			r += q
		}
	}
	return r
}
