module math

fn JS.Math.ceil(x f64) f64

fn JS.Math.floor(x f64) f64

fn JS.Math.round(x f64) f64

fn JS.Math.trunc(x f64) f64

// ceil returns the nearest f64 greater or equal to the provided value.
[inline]
pub fn ceil(x f64) f64 {
	return JS.Math.ceil(x)
}

// floor returns the nearest f64 lower or equal of the provided value.
[inline]
pub fn floor(x f64) f64 {
	return JS.Math.floor(x)
}

// round returns the integer nearest to the provided value.
[inline]
pub fn round(x f64) f64 {
	return JS.Math.round(x)
}

// trunc rounds a toward zero, returning the nearest integral value that is not
// larger in magnitude than a.
[inline]
pub fn trunc(x f64) f64 {
	return JS.Math.trunc(x)
}
