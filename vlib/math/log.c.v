module math

fn C.log(x f64) f64
fn C.log2(x f64) f64
fn C.log10(x f64) f64
fn C.log1p(x f64) f64
fn C.logb(x f64) f64
fn C.logf(x f32) f32

// log returns the natural logarithm of x (float64)
@[inline]
pub fn log(x f64) f64 {
	return C.log(x)
}

// log2 returns the binary logarithm of x (float64).
// The special cases are the same as for log.
@[inline]
pub fn log2(x f64) f64 {
	return C.log2(x)
}

// log10 returns the decimal logarithm of x (float64).
// The special cases are the same as for log.
@[inline]
pub fn log10(x f64) f64 {
	return C.log10(x)
}

// log1p returns log(1+x).
@[inline]
pub fn log1p(x f64) f64 {
	return C.log1p(x)
}

// log_b returns the binary exponent of x.
pub fn log_b(x f64) f64 {
	return C.logb(x)
}

// log returns the natural logarithm of x (float32)
@[inline]
pub fn logf(x f32) f32 {
	return C.logf(x)
}
