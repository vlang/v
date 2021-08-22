module math

fn C.erf(x f64) f64

fn C.erfc(x f64) f64

// erf computes the error function value
[inline]
pub fn erf(a f64) f64 {
	return C.erf(a)
}

// erfc computes the complementary error function value
[inline]
pub fn erfc(a f64) f64 {
	return C.erfc(a)
}
