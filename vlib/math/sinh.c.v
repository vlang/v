module math

fn C.cosh(x f64) f64

fn C.sinh(x f64) f64

// cosh calculates hyperbolic cosine.
[inline]
pub fn cosh(a f64) f64 {
	return C.cosh(a)
}

// sinh calculates hyperbolic sine.
[inline]
pub fn sinh(a f64) f64 {
	return C.sinh(a)
}
