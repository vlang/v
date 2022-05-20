module math

fn JS.Math.cosh(x f64) f64

fn JS.Math.sinh(x f64) f64

// cosh calculates hyperbolic cosine in radians
[inline]
pub fn cosh(a f64) f64 {
	return JS.Math.cosh(a)
}

// sinh calculates hyperbolic sine in radians
[inline]
pub fn sinh(a f64) f64 {
	return JS.Math.sinh(a)
}
