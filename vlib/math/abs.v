module math

// Returns the absolute value.
[inline]
pub fn abs(x f64) f64 {
	if x > 0.0 {
		return x
	}
	return -x
}

[inline]
pub fn fabs(x f64) f64 {
	if x > 0.0 {
		return x
	}
	return -x
}
