module math

// lgamma returns the natural logarithm of the absolute value of Gamma(x).
// It is an alias of log_gamma, including its NaN, infinity, and pole behavior.
pub fn lgamma(x f64) f64 {
	return log_gamma(x)
}

// remainder returns the nearest-integer, ties-to-even remainder without
// dividing x by y, preserving finite inputs even when their quotient overflows.
pub fn remainder(x f64, y f64) f64 {
	if is_nan(x) || is_nan(y) || y == 0 || is_inf(x, 0) { return nan() }
	if is_inf(y, 0) { return x }
	divisor := abs(y)
	mut r := abs(fmod(x, divisor))
	if r == 0 { return copysign(0.0, x) }
	other := divisor - r
	if r > other || (r == other && abs(fmod(x, 2.0 * divisor)) > divisor) {
		r -= divisor
	}
	return if signbit(x) { -r } else { r }
}

// fma returns x*y + z. The C backend overrides this with a single-rounded
// math-library operation; other backends retain two ordinary rounded steps.
pub fn fma(x f64, y f64, z f64) f64 {
	return x * y + z
}

// floor_mod returns x mod y floored toward negative infinity, so the result
// has the sign of y, including an exact zero (Python's floating-point %).
pub fn floor_mod(x f64, y f64) f64 {
	if y == 0 || x != x || y != y {
		return nan()
	}
	r := fmod(x, y)
	if r == 0 {
		return copysign(0.0, y)
	}
	if (r < 0) != (y < 0) {
		return r + y
	}
	return r
}

// floor_div returns x div y floored toward negative infinity, the quotient
// counterpart of floor_mod, so that floor_div(x, y) * y + floor_mod(x, y) == x
// (up to floating-point rounding), matching Python's floating-point //.
pub fn floor_div(x f64, y f64) f64 {
	if y == 0 || x != x || y != y {
		return nan()
	}
	return floor(x / y)
}

// to_degrees converts radians to degrees: x * 180 / pi.
pub fn to_degrees(radians f64) f64 {
	return radians * 180.0 / pi
}

// to_radians converts degrees to radians: x * pi / 180.
pub fn to_radians(degrees f64) f64 {
	return degrees * pi / 180.0
}

// is_power_of_two reports whether n is a positive power of two (0 is not).
pub fn is_power_of_two(n u64) bool {
	return n != 0 && (n & (n - 1)) == 0
}

// next_power_of_two returns the smallest power of two >= n (1 for n == 0).
// Results above 2^63 saturate to 0, since 2^64 would not fit in u64.
pub fn next_power_of_two(n u64) u64 {
	if n <= 1 {
		return 1
	}
	mut v := n - 1
	v |= v >> 1
	v |= v >> 2
	v |= v >> 4
	v |= v >> 8
	v |= v >> 16
	v |= v >> 32
	// v is now 2^k - 1 for the enclosing power of two; adding one either
	// yields that power (fits) or overflows to 0 for 2^64.
	return v + 1
}

// FloatClass classifies a floating-point value, as Go's math.Float64bits
// inspection or Rust's classify do.
pub enum FloatClass {
	nan
	infinite
	normal
	subnormal
	zero
}

// float_ops_smallest_normal is the smallest positive normal f64 (2^-1022).
const float_ops_smallest_normal = 2.2250738585072014e-308

// classify reports the class of x.
pub fn classify(x f64) FloatClass {
	if x != x {
		return .nan
	}
	if x == inf(1) || x == inf(-1) {
		return .infinite
	}
	a := abs(x)
	if a == 0 {
		return .zero
	}
	if a < float_ops_smallest_normal {
		return .subnormal
	}
	return .normal
}

// dim returns the maximum of x-y and 0, with NaN propagation (Go's math.Dim).
pub fn dim(x f64, y f64) f64 {
	if is_nan(x) || is_nan(y) {
		return nan()
	}
	v := x - y
	if v <= 0 {
		return 0
	}
	return v
}
