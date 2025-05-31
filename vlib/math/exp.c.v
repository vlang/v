module math

fn C.exp(x f64) f64

fn C.exp2(x f64) f64

fn C.ldexp(x f64, exp int) f64

// exp returns e**x, the base-e exponential of x.
@[inline]
pub fn exp(x f64) f64 {
	return C.exp(x)
}

// exp2 returns 2**x, the base-2 exponential of x.
@[inline]
pub fn exp2(x f64) f64 {
	return C.exp2(x)
}

// ldexp calculates frac*(2**exp).
@[inline]
pub fn ldexp(frac f64, exp int) f64 {
	return C.ldexp(frac, exp)
}
