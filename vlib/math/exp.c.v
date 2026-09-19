module math

fn C.exp(x f64) f64

fn C.exp2(x f64) f64

fn C.ldexp(x f64, exp i32) f64

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
	// C.ldexp only accepts a 32-bit exponent, while V's int can be 64-bit.
	if exp < int(min_i32) || exp > int(max_i32) {
		return scalbn(frac, exp)
	}
	return C.ldexp(frac, i32(exp))
}
