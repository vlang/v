module math

// gamma function computed by Stirling's formula.
// The pair of results must be multiplied together to get the actual answer.
// The multiplication is left to the caller so that, if careful, the caller can avoid
// infinity for 172 <= x <= 180.
// The polynomial is valid for 33 <= x <= 172 larger values are only used
// in reciprocal and produce denormalized floats. The lower precision there
// masks any imprecision in the polynomial.
fn stirling(x f64) (f64, f64) {
	if x > 200 {
		return inf(1), 1.0
	}
	sqrt_two_pi := 2.506628274631000502417
	max_stirling := 143.01608
	mut w := 1.0 / x
	w = 1.0 + w * ((((gamma_s[0] * w + gamma_s[1]) * w + gamma_s[2]) * w + gamma_s[3]) * w +
		gamma_s[4])
	mut y1 := exp(x)
	mut y2 := 1.0
	if x > max_stirling { // avoid Pow() overflow
		v := pow(x, 0.5 * x - 0.25)
		y1_ := y1
		y1 = v
		y2 = v / y1_
	} else {
		y1 = pow(x, x - 0.5) / y1
	}
	return y1, f64(sqrt_two_pi) * w * y2
}

// gamma returns the gamma function of x.
//
// special ifs are:
// gamma(+inf) = +inf
// gamma(+0) = +inf
// gamma(-0) = -inf
// gamma(x) = nan for integer x < 0
// gamma(-inf) = nan
// gamma(nan) = nan
pub fn gamma(a f64) f64 {
	mut x := a
	euler := 0.57721566490153286060651209008240243104215933593992 // A001620
	if is_neg_int(x) || is_inf(x, -1) || is_nan(x) {
		return nan()
	}
	if is_inf(x, 1) {
		return inf(1)
	}
	if x == 0.0 {
		return copysign(inf(1), x)
	}
	mut q := abs(x)
	mut p := floor(q)
	if q > 33 {
		if x >= 0 {
			y1, y2 := stirling(x)
			return y1 * y2
		}
		// Note: x is negative but (checked above) not a negative integer,
		// so x must be small enough to be in range for conversion to i64.
		// If |x| were >= 2⁶³ it would have to be an integer.
		mut signgam := 1
		ip := i64(p)
		if (ip & 1) == 0 {
			signgam = -1
		}
		mut z := q - p
		if z > 0.5 {
			p = p + 1
			z = q - p
		}
		z = q * sin(pi * z)
		if z == 0 {
			return inf(signgam)
		}
		sq1, sq2 := stirling(q)
		absz := abs(z)
		d := absz * sq1 * sq2
		if is_inf(d, 0) {
			z = pi / absz / sq1 / sq2
		} else {
			z = pi / d
		}
		return f64(signgam) * z
	}
	// Reduce argument
	mut z := 1.0
	for x >= 3 {
		x = x - 1
		z = z * x
	}
	for x < 0 {
		if x > -1e-09 {
			unsafe {
				goto _small
			}
		}
		z = z / x
		x = x + 1
	}
	for x < 2 {
		if x < 1e-09 {
			unsafe {
				goto _small
			}
		}
		z = z / x
		x = x + 1
	}
	if x == 2 {
		return z
	}
	x = x - 2
	p = (((((x * gamma_p[0] + gamma_p[1]) * x + gamma_p[2]) * x + gamma_p[3]) * x +
		gamma_p[4]) * x + gamma_p[5]) * x + gamma_p[6]
	q = ((((((x * gamma_q[0] + gamma_q[1]) * x + gamma_q[2]) * x + gamma_q[3]) * x +
		gamma_q[4]) * x + gamma_q[5]) * x + gamma_q[6]) * x + gamma_q[7]
	if true {
		return z * p / q
	}
	_small:
	if x == 0 {
		return inf(1)
	}
	return z / ((1.0 + euler * x) * x)
}

// log_gamma returns the natural logarithm and sign (-1 or +1) of Gamma(x).
//
// special ifs are:
// log_gamma(+inf) = +inf
// log_gamma(0) = +inf
// log_gamma(-integer) = +inf
// log_gamma(-inf) = -inf
// log_gamma(nan) = nan
pub fn log_gamma(x f64) f64 {
	y, _ := log_gamma_sign(x)
	return y
}

pub fn log_gamma_sign(a f64) (f64, int) {
	mut x := a
	ymin := 1.461632144968362245
	tiny := exp2(-70)
	two52 := exp2(52) // 0x4330000000000000 ~4.5036e+15
	two58 := exp2(58) // 0x4390000000000000 ~2.8823e+17
	tc := 1.46163214496836224576e+00 // 0x3FF762D86356BE3F
	tf := -1.21486290535849611461e-01 // 0xBFBF19B9BCC38A42
	// tt := -(tail of tf)
	tt := -3.63867699703950536541e-18 // 0xBC50C7CAA48A971F
	mut sign := 1
	if is_nan(x) {
		return x, sign
	}
	if is_inf(x, 1) {
		return x, sign
	}
	if x == 0.0 {
		return inf(1), sign
	}
	mut neg := false
	if x < 0 {
		x = -x
		neg = true
	}
	if x < tiny { // if |x| < 2**-70, return -log(|x|)
		if neg {
			sign = -1
		}
		return -log(x), sign
	}
	mut nadj := 0.0
	if neg {
		if x >= two52 {
			// x| >= 2**52, must be -integer
			return inf(1), sign
		}
		t := sin_pi(x)
		if t == 0 {
			return inf(1), sign
		}
		nadj = log(pi / abs(t * x))
		if t < 0 {
			sign = -1
		}
	}
	mut lgamma := 0.0
	if x == 1 || x == 2 { // purge off 1 and 2
		return 0.0, sign
	} else if x < 2 { // use lgamma(x) = lgamma(x+1) - log(x)
		mut y := 0.0
		mut i := 0
		if x <= 0.9 {
			lgamma = -log(x)
			if x >= (ymin - 1 + 0.27) { // 0.7316 <= x <=  0.9
				y = 1.0 - x
				i = 0
			} else if x >= (ymin - 1 - 0.27) { // 0.2316 <= x < 0.7316
				y = x - (tc - 1)
				i = 1
			} else { // 0 < x < 0.2316
				y = x
				i = 2
			}
		} else {
			lgamma = 0
			if x >= (ymin + 0.27) { // 1.7316 <= x < 2
				y = f64(2) - x
				i = 0
			} else if x >= (ymin - 0.27) { // 1.2316 <= x < 1.7316
				y = x - tc
				i = 1
			} else { // 0.9 < x < 1.2316
				y = x - 1
				i = 2
			}
		}
		if i == 0 {
			z := y * y
			gamma_p1 := lgamma_a[0] + z * (lgamma_a[2] + z * (lgamma_a[4] + z * (lgamma_a[6] +
				z * (lgamma_a[8] + z * lgamma_a[10]))))
			gamma_p2 := z * (lgamma_a[1] + z * (lgamma_a[3] + z * (lgamma_a[5] + z * (lgamma_a[7] +
				z * (lgamma_a[9] + z * lgamma_a[11])))))
			p := y * gamma_p1 + gamma_p2
			lgamma += (p - 0.5 * y)
		} else if i == 1 {
			z := y * y
			w := z * y
			gamma_p1 := lgamma_t[0] + w * (lgamma_t[3] + w * (lgamma_t[6] + w * (lgamma_t[9] +
				w * lgamma_t[12]))) // parallel comp
			gamma_p2 := lgamma_t[1] + w * (lgamma_t[4] + w * (lgamma_t[7] + w * (lgamma_t[10] +
				w * lgamma_t[13])))
			gamma_p3 := lgamma_t[2] + w * (lgamma_t[5] + w * (lgamma_t[8] + w * (lgamma_t[11] +
				w * lgamma_t[14])))
			p := z * gamma_p1 - (tt - w * (gamma_p2 + y * gamma_p3))
			lgamma += (tf + p)
		} else if i == 2 {
			gamma_p1 := y * (lgamma_u[0] + y * (lgamma_u[1] + y * (lgamma_u[2] + y * (lgamma_u[3] +
				y * (lgamma_u[4] + y * lgamma_u[5])))))
			gamma_p2 := 1.0 + y * (lgamma_v[1] + y * (lgamma_v[2] + y * (lgamma_v[3] +
				y * (lgamma_v[4] + y * lgamma_v[5]))))
			lgamma += (-0.5 * y + gamma_p1 / gamma_p2)
		}
	} else if x < 8 { // 2 <= x < 8
		i := int(x)
		y := x - f64(i)
		p := y * (lgamma_s[0] + y * (lgamma_s[1] + y * (lgamma_s[2] + y * (lgamma_s[3] +
			y * (lgamma_s[4] + y * (lgamma_s[5] + y * lgamma_s[6]))))))
		q := 1.0 + y * (lgamma_r[1] + y * (lgamma_r[2] + y * (lgamma_r[3] + y * (lgamma_r[4] +
			y * (lgamma_r[5] + y * lgamma_r[6])))))
		lgamma = 0.5 * y + p / q
		mut z := 1.0 // lgamma(1+s) = log(s) + lgamma(s)
		if i == 7 {
			z *= (y + 6)
			z *= (y + 5)
			z *= (y + 4)
			z *= (y + 3)
			z *= (y + 2)
			lgamma += log(z)
		} else if i == 6 {
			z *= (y + 5)
			z *= (y + 4)
			z *= (y + 3)
			z *= (y + 2)
			lgamma += log(z)
		} else if i == 5 {
			z *= (y + 4)
			z *= (y + 3)
			z *= (y + 2)
			lgamma += log(z)
		} else if i == 4 {
			z *= (y + 3)
			z *= (y + 2)
			lgamma += log(z)
		} else if i == 3 {
			z *= (y + 2)
			lgamma += log(z)
		}
	} else if x < two58 { // 8 <= x < 2**58
		t := log(x)
		z := 1.0 / x
		y := z * z
		w := lgamma_w[0] + z * (lgamma_w[1] + y * (lgamma_w[2] + y * (lgamma_w[3] +
			y * (lgamma_w[4] + y * (lgamma_w[5] + y * lgamma_w[6])))))
		lgamma = (x - 0.5) * (t - 1.0) + w
	} else { // 2**58 <= x <= Inf
		lgamma = x * (log(x) - 1.0)
	}
	if neg {
		lgamma = nadj - lgamma
	}
	return lgamma, sign
}

// sin_pi(x) is a helper function for negative x
fn sin_pi(x_ f64) f64 {
	mut x := x_
	two52 := exp2(52) // 0x4330000000000000 ~4.5036e+15
	two53 := exp2(53) // 0x4340000000000000 ~9.0072e+15
	if x < 0.25 {
		return -sin(pi * x)
	}
	// argument reduction
	mut z := floor(x)
	mut n := 0
	if z != x { // inexact
		x = mod(x, 2)
		n = int(x * 4)
	} else {
		if x >= two53 { // x must be even
			x = 0
			n = 0
		} else {
			if x < two52 {
				z = x + two52 // exact
			}
			n = 1 & int(f64_bits(z))
			x = f64(n)
			n <<= 2
		}
	}
	if n == 0 {
		x = sin(pi * x)
	} else if n == 1 || n == 2 {
		x = cos(pi * (0.5 - x))
	} else if n == 3 || n == 4 {
		x = sin(pi * (1.0 - x))
	} else if n == 5 || n == 6 {
		x = -cos(pi * (x - 1.5))
	} else {
		x = sin(pi * (x - 2))
	}
	return -x
}
