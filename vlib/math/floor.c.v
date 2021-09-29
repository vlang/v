module math

fn C.ceil(x f64) f64

fn C.floor(x f64) f64

fn C.round(x f64) f64

fn C.trunc(x f64) f64

// ceil returns the nearest f64 greater or equal to the provided value.
[inline]
pub fn ceil(x f64) f64 {
	return C.ceil(x)
}

// floor returns the nearest f64 lower or equal of the provided value.
[inline]
pub fn floor(x f64) f64 {
	return C.floor(x)
}

// round returns the integer nearest to the provided value.
[inline]
pub fn round(x f64) f64 {
	return C.round(x)
}

// trunc rounds a toward zero, returning the nearest integral value that is not
// larger in magnitude than a.
[inline]
pub fn trunc(x f64) f64 {
	return C.trunc(x)
}
