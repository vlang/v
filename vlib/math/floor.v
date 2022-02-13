module math

// floor returns the greatest integer value less than or equal to x.
//
// special cases are:
// floor(±0) = ±0
// floor(±inf) = ±inf
// floor(nan) = nan
pub fn floor(x f64) f64 {
	if x == 0 || is_nan(x) || is_inf(x, 0) {
		return x
	}
	if x < 0 {
		mut d, fract := modf(-x)
		if fract != 0.0 {
			d = d + 1
		}
		return -d
	}
	d, _ := modf(x)
	return d
}

// ceil returns the least integer value greater than or equal to x.
//
// special cases are:
// ceil(±0) = ±0
// ceil(±inf) = ±inf
// ceil(nan) = nan
pub fn ceil(x f64) f64 {
	return -floor(-x)
}

// trunc returns the integer value of x.
//
// special cases are:
// trunc(±0) = ±0
// trunc(±inf) = ±inf
// trunc(nan) = nan
pub fn trunc(x f64) f64 {
	if x == 0 || is_nan(x) || is_inf(x, 0) {
		return x
	}
	d, _ := modf(x)
	return d
}

// round returns the nearest integer, rounding half away from zero.
//
// special cases are:
// round(±0) = ±0
// round(±inf) = ±inf
// round(nan) = nan
pub fn round(x f64) f64 {
	if x == 0 || is_nan(x) || is_inf(x, 0) {
		return x
	}
	// Largest integer <= x
	mut y := floor(x) // Fractional part
	mut r := x - y // Round up to nearest.
	if r > 0.5 {
		unsafe {
			goto rndup
		}
	}
	// Round to even
	if r == 0.5 {
		r = y - 2.0 * floor(0.5 * y)
		if r == 1.0 {
			rndup:
			y += 1.0
		}
	}
	// Else round down.
	return y
}

// round_to_even returns the nearest integer, rounding ties to even.
//
// special cases are:
// round_to_even(±0) = ±0
// round_to_even(±inf) = ±inf
// round_to_even(nan) = nan
pub fn round_to_even(x f64) f64 {
	mut bits := f64_bits(x)
	mut e_ := (bits >> shift) & mask
	if e_ >= bias {
		// round abs(x) >= 1.
		// - Large numbers without fractional components, infinity, and nan are unchanged.
		// - Add 0.499.. or 0.5 before truncating depending on whether the truncated
		// number is even or odd (respectively).
		half_minus_ulp := u64(u64(1) << (shift - 1)) - 1
		e_ -= u64(bias)
		bits += (half_minus_ulp + (bits >> (shift - e_)) & 1) >> e_
		bits &= frac_mask >> e_
		bits ^= frac_mask >> e_
	} else if e_ == bias - 1 && bits & frac_mask != 0 {
		// round 0.5 < abs(x) < 1.
		bits = bits & sign_mask | uvone // +-1
	} else {
		// round abs(x) <= 0.5 including denormals.
		bits &= sign_mask // +-0
	}
	return f64_from_bits(bits)
}
