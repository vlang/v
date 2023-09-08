module math

fn C.log(x f64) f64

fn C.logf(x f32) f32

// log returns the natural logarithm of x (float64)
[inline]
pub fn log(a f64) f64 {
	return C.log(a)
}

// log returns the natural logarithm of x (float32)
[inline]
pub fn logf(a f32) f32 {
	return C.logf(a)
}
