module math

fn C.exp(x f64) f64

fn C.exp2(x f64) f64

// exp calculates exponent of the number (math.pow(math.E, x)).
[inline]
pub fn exp(x f64) f64 {
	return C.exp(x)
}

// exp2 returns the base-2 exponential function of a (math.pow(2, x)).
[inline]
pub fn exp2(x f64) f64 {
	return C.exp2(x)
}
