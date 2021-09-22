module math

pub fn log_n(x f64, b f64) f64 {
	y := log(x)
	z := log(b)
	return y / z
}

// log10 returns the decimal logarithm of x.
// The special cases are the same as for log.
pub fn log10(x f64) f64 {
	return log(x) * (1.0 / ln10)
}

// log2 returns the binary logarithm of x.
// The special cases are the same as for log.
pub fn log2(x f64) f64 {
	frac, exp := frexp(x)
	// Make sure exact powers of two give an exact answer.
	// Don't depend on log(0.5)*(1/ln2)+exp being exactly exp-1.
	if frac == 0.5 {
		return f64(exp - 1)
	}
	return log(frac) * (1.0 / ln2) + f64(exp)
}

pub fn log1p(x f64) f64 {
	y := 1.0 + x
	z := y - 1.0
	return log(y) - (z - x) / y // cancels errors with IEEE arithmetic
}

// log_b returns the binary exponent of x.
//
// special cases are:
// log_b(±inf) = +inf
// log_b(0) = -inf
// log_b(nan) = nan
pub fn log_b(x f64) f64 {
	if x == 0 {
		return inf(-1)
	}
	if is_inf(x, 0) {
		return inf(1)
	}
	if is_nan(x) {
		return x
	}
	return f64(ilog_b_(x))
}

// ilog_b returns the binary exponent of x as an integer.
//
// special cases are:
// ilog_b(±inf) = max_i32
// ilog_b(0) = min_i32
// ilog_b(nan) = max_i32
pub fn ilog_b(x f64) int {
	if x == 0 {
		return min_i32
	}
	if is_nan(x) {
		return max_i32
	}
	if is_inf(x, 0) {
		return max_i32
	}
	return ilog_b_(x)
}

// ilog_b returns the binary exponent of x. It assumes x is finite and
// non-zero.
fn ilog_b_(x_ f64) int {
	x, exp := normalize(x_)
	return int((f64_bits(x) >> shift) & mask) - bias + exp
}
