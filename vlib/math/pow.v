module math

const pow10tab = [f64(1e+00), 1e+01, 1e+02, 1e+03, 1e+04, 1e+05, 1e+06, 1e+07, 1e+08, 1e+09, 1e+10,
	1e+11, 1e+12, 1e+13, 1e+14, 1e+15, 1e+16, 1e+17, 1e+18, 1e+19, 1e+20, 1e+21, 1e+22, 1e+23,
	1e+24, 1e+25, 1e+26, 1e+27, 1e+28, 1e+29, 1e+30, 1e+31]
const pow10postab32 = [f64(1e+00), 1e+32, 1e+64, 1e+96, 1e+128, 1e+160, 1e+192, 1e+224, 1e+256,
	1e+288]
const pow10negtab32 = [f64(1e-00), 1e-32, 1e-64, 1e-96, 1e-128, 1e-160, 1e-192, 1e-224, 1e-256,
	1e-288, 1e-320]

// powf returns base raised to the provided power. (float32)
@[inline]
pub fn powf(a f32, b f32) f32 {
	return f32(pow(a, b))
}

// pow10 returns 10**n, the base-10 exponential of n.
//
// special cases are:
// - `pow10(n) =    0 for n < -323`
// - `pow10(n) = +inf for n > 308`
pub fn pow10(n int) f64 {
	if 0 <= n && n <= 308 {
		return pow10postab32[u32(n) / 32] * pow10tab[u32(n) % 32]
	}
	if -323 <= n && n <= 0 {
		return pow10negtab32[u32(-n) / 32] / pow10tab[u32(-n) % 32]
	}
	// n < -323 || 308 < n
	if n > 0 {
		return inf(1)
	}
	// n < -323
	return 0.0
}

// powi returns base raised to power (a**b) as an integer (i64)
//
// A negative exponent returns 0 for nonzero bases outside 1 and -1.
// special case:
// powi(a, b) = -1 for a = 0 and b < 0
pub fn powi(a i64, b i64) i64 {
	mut b_ := b
	mut p := a
	mut v := i64(1)

	if b_ < 0 { // exponent < 0
		if a == 0 {
			return -1 // division by 0
		}
		if a == 1 || a == -1 {
			return if b_ & 1 > 0 { a } else { 1 }
		}
		return 0
	}

	for ; b_ > 0; {
		if b_ & 1 > 0 {
			v *= p
		}
		b_ >>= 1
		if b_ > 0 {
			p *= p
		}
	}

	return v
}

fn is_odd_int(x f64) bool {
	// Every binary64 integer at or above 2^53 is even. Bound the cast as
	// well: converting a larger finite value to i64 is undefined in C.
	if abs(x) >= 9007199254740992.0 {
		return false
	}
	xi, xf := modf(x)
	return xf == 0 && (i64(xi) & 1) == 1
}

// pow_multiply keeps the product roundoff while accumulating normalized
// powers. A single-word product loses accuracy when a base close to one is
// squared many times and its early roundoff is amplified by a large exponent.
fn pow_multiply(ah f64, al f64, bh f64, bl f64) (f64, f64) {
	p := ah * bh
	roundoff := fma(ah, bh, -p) + (ah * bl + al * bh) + al * bl
	h := p + roundoff
	return h, roundoff - (h - p)
}

// pow returns the base x, raised to the provided power y. (float64)
//
// todo(playXE): make this function work on JS backend, probably problem of JS codegen that it does not work.
pub fn pow(x f64, y f64) f64 {
	return pow_software(x, y)
}

fn pow_software(x f64, y f64) f64 {
	if y == 0 || x == 1 {
		return 1
	} else if y == 1 {
		return x
	} else if is_nan(x) || is_nan(y) {
		return nan()
	} else if y == 2 {
		return x * x
	} else if y == 3 {
		return x * x * x
	} else if x == 0 {
		if y < 0 {
			if is_odd_int(y) {
				return copysign(inf(1), x)
			}
			return inf(1)
		} else if y > 0 {
			if is_odd_int(y) {
				return x
			}
			return 0
		}
	} else if is_inf(y, 0) {
		if x == -1 {
			return 1
		} else if (abs(x) < 1) == is_inf(y, 1) {
			return 0
		} else {
			return inf(1)
		}
	} else if is_inf(x, 0) {
		if is_inf(x, -1) {
			return pow_software(1 / x, -y)
		}

		if y < 0 {
			return 0
		} else if y > 0 {
			return inf(1)
		}
	} else if y == 0.5 {
		return sqrt(x)
	} else if y == -0.5 {
		return 1 / sqrt(x)
	}
	mut yi, mut yf := modf(abs(y))

	if yf != 0 && x < 0 {
		return nan()
	}
	if yi >= (u64(1) << 63) {
		// yi is a large even int that will lead to overflow (or underflow to 0)
		// for all x except -1 (x == 1 was handled earlier)

		if x == -1 {
			return 1
		} else if (abs(x) < 1) == (y > 0) {
			return 0
		} else {
			return inf(1)
		}
	}

	// ans = a1 * 2**ae (= 1 for now).
	mut a1 := 1.0
	mut a1_low := 0.0
	mut ae := 0
	mut x1, mut xe := frexp(x)

	// ans *= x**yf
	if yf != 0 {
		if yf > 0.5 {
			yf--
			yi++
		}

		// Reduce the fractional power before taking log. Rounding log(x)
		// first would lose low bits of a large binary exponent, which exp
		// then amplifies even for a small fractional power.
		scale_up := x1 < sqrt2 / 2
		fraction := if scale_up { x1 * 2 } else { x1 }
		exponent := if scale_up { xe - 1 } else { xe }
		scale := yf * f64(exponent)
		scale_error := fma(yf, f64(exponent), -scale)
		scale_integer, scale_fraction := modf(scale)
		ae = int(scale_integer)
		a1, a1_low = pow_multiply(exp(yf * log(fraction)), 0,
			exp2(scale_fraction + scale_error), 0)
	}

	// ans *= x**yi
	// by multiplying in successive squarings
	// of x according to bits of yi.
	// accumulate powers of two into exp.
	mut x1_low := 0.0

	for i := i64(yi); i != 0; i >>= 1 {
		// Negate a positive shift so the lower bound stays negative for any int width.
		if xe < -(1 << 12) || 1 << 12 < xe {
			// catch xe before it overflows the left shift below
			// Since i !=0 it has at least one bit still set, so ae will accumulate xe
			// on at least one more iteration, ae += xe is a lower bound on ae
			// the lower bound on ae exceeds the size of a float64 exp
			// so the final call to Ldexp will produce under/overflow (0/Inf)
			ae += xe
			break
		}
		if i & 1 == 1 {
			a1, a1_low = pow_multiply(a1, a1_low, x1, x1_low)
			ae += xe
		}
		x1, x1_low = pow_multiply(x1, x1_low, x1, x1_low)
		xe *= 2
		if x1 < .5 {
			x1 += x1
			x1_low += x1_low
			xe--
		}
	}

	// ans = a1*2**ae
	// if y < 0 { ans = 1 / ans }
	// but in the opposite order
	if y < 0 {
		r := 1 / a1
		correction := (fma(-r, a1, 1) - r * a1_low) / a1
		a1 = r
		a1_low = correction
		ae = -ae
	}
	return ldexp(a1 + a1_low, ae)
}
