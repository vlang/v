// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module specfunc

import vsl.math
// gamma function computed by Stirling's formula.
// The pair of results must be multiplied together to get the actual answer.
// The multiplication is left to the caller so that, if careful, the caller can avoid
// infinity for 172 <= x <= 180.
// The polynomial is valid for 33 <= x <= 172 larger values are only used
// in reciprocal and produce denormalized floats. The lower precision there
// masks any imprecision in the polynomial.
fn stirling(x f64) (f64,f64) {
	if x > 200 {
		return math.inf(1),f64(1.0)
	}
	sqrt_two_pi := 2.506628274631000502417
	max_stirling := 143.01608
	mut w := f64(1) / x
	w = f64(1) + w * ((((GAMMA_S[0] * w + GAMMA_S[1]) * w + GAMMA_S[2]) * w + GAMMA_S[3]) * w + GAMMA_S[4])
	mut y1 := exp(x)
	mut y2 := f64(1)
	if x > max_stirling {
		// avoid Pow() overflow
		v := pow(x, 0.5 * x - 0.25)
		y1_ := y1
		y1 = v
		y2 = v / y1_
	}
	else {
		y1 = math.pow(x, x - 0.5) / y1
	}
	return y1,f64(sqrt_two_pi) * w * y2
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
pub fn gamma(x_ f64) f64 {
	mut x := x_
	euler := 0.57721566490153286060651209008240243104215933593992 // A001620
	if is_neg_int(x) || math.is_inf(x, -1) || math.is_nan(x) {
		return math.nan()
	}
	if math.is_inf(x, 1) {
		return math.inf(1)
	}
	if x == 0.0 {
		return math.copysign(math.inf(1), x)
	}
	mut q := math.abs(x)
	mut p := math.floor(q)
	if q > 33 {
		if x >= 0 {
			y1,y2 := stirling(x)
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
		z = q * math.sin(math.pi * z)
		if z == 0 {
			return math.inf(signgam)
		}
		sq1,sq2 := stirling(q)
		absz := math.abs(z)
		d := absz * sq1 * sq2
		if math.is_inf(d, 0) {
			z = math.pi / absz / sq1 / sq2
		}
		else {
			z = math.pi / d
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
			goto small
		}
		z = z / x
		x = x + 1
	}
	for x < 2 {
		if x < 1e-09 {
			goto small
		}
		z = z / x
		x = x + 1
	}
	if x == 2 {
		return z
	}
	x = x - 2
	p = (((((x * GAMMA_P[0] + GAMMA_P[1]) * x + GAMMA_P[2]) * x + GAMMA_P[3]) * x + GAMMA_P[4]) * x + GAMMA_P[5]) * x + GAMMA_P[6]
	q = ((((((x * GAMMA_Q[0] + GAMMA_Q[1]) * x + GAMMA_Q[2]) * x + GAMMA_Q[3]) * x + GAMMA_Q[4]) * x + GAMMA_Q[5]) * x + GAMMA_Q[6]) * x + GAMMA_Q[7]
	if true {
		return z * p / q
	}
small:
	if x == 0 {
		return math.inf(1)
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
	y,_ := log_gamma_sign(x)
	return y
}

pub fn log_gamma_sign(x_ f64) (f64,int) {
	mut x := x_
	ymin := 1.461632144968362245
	tiny := math.exp2(-70)
	two52 := math.exp2(52) // 0x4330000000000000 ~4.5036e+15
	two58 := math.exp2(58) // 0x4390000000000000 ~2.8823e+17
	tc := 1.46163214496836224576e+00 // 0x3FF762D86356BE3F
	tf := -1.21486290535849611461e-01 // 0xBFBF19B9BCC38A42
	// tt := -(tail of tf)
	tt := -3.63867699703950536541e-18 // 0xBC50C7CAA48A971F
	mut sign := 1
	if math.is_nan(x) {
		return x,sign
	}
	if math.is_inf(x, 1) {
		return x,sign
	}
	if x == 0.0 {
		return math.inf(1),sign
	}
	mut neg := false
	if x < 0 {
		x = -x
		neg = true
	}
	if x < tiny {
		// if |x| < 2**-70, return -log(|x|)
		if neg {
			sign = -1
		}
		return -math.log(x),sign
	}
	mut nadj := f64(0)
	if neg {
		if x >= two52 {
			// |x| >= 2**52, must be -integer
			return math.inf(1),sign
		}
		t := sin_pi(x)
		if t == 0 {
			return math.inf(1),sign
		}
		nadj = math.log(math.pi / math.abs(t * x))
		if t < 0 {
			sign = -1
		}
	}
	mut lgamma := f64(0)
	if x == 1 || x == 2 {
		// purge off 1 and 2
		return f64(0),sign
	}
	else if x < 2 {
		// use lgamma(x) = lgamma(x+1) - log(x)
		mut y := f64(0)
		mut i := 0
		if x <= 0.9 {
			lgamma = -log(x)
			if x >= (ymin - 1 + 0.27) {
				// 0.7316 <= x <=  0.9
				y = f64(1) - x
				i = 0
			}
			else if x >= (ymin - 1 - 0.27) {
				// 0.2316 <= x < 0.7316
				y = x - (tc - 1)
				i = 1
			}
			else {
				// 0 < x < 0.2316
				y = x
				i = 2
			}
		}
		else {
			lgamma = 0
			if x >= (ymin + 0.27) {
				// 1.7316 <= x < 2
				y = f64(2) - x
				i = 0
			}
			else if x >= (ymin - 0.27) {
				// 1.2316 <= x < 1.7316
				y = x - tc
				i = 1
			}
			else {
				// 0.9 < x < 1.2316
				y = x - 1
				i = 2
			}
		}
		if i == 0 {
			z := y * y
			p1 := LGAMMA_A[0] + z * (LGAMMA_A[2] + z * (LGAMMA_A[4] + z * (LGAMMA_A[6] + z * (LGAMMA_A[8] + z * LGAMMA_A[10]))))
			p2 := z * (LGAMMA_A[1] + z * (LGAMMA_A[3] + z * (LGAMMA_A[5] + z * (LGAMMA_A[7] + z * (LGAMMA_A[9] + z * LGAMMA_A[11])))))
			p := y * p1 + p2
			lgamma += (p - 0.5 * y)
		}
		else if i == 1 {
			z := y * y
			w := z * y
			p1 := LGAMMA_T[0] + w * (LGAMMA_T[3] + w * (LGAMMA_T[6] + w * (LGAMMA_T[9] + w * LGAMMA_T[12]))) // parallel comp
			p2 := LGAMMA_T[1] + w * (LGAMMA_T[4] + w * (LGAMMA_T[7] + w * (LGAMMA_T[10] + w * LGAMMA_T[13])))
			p3 := LGAMMA_T[2] + w * (LGAMMA_T[5] + w * (LGAMMA_T[8] + w * (LGAMMA_T[11] + w * LGAMMA_T[14])))
			p := z * p1 - (tt - w * (p2 + y * p3))
			lgamma += (tf + p)
		}
		else if i == 2 {
			p1 := y * (LGAMMA_U[0] + y * (LGAMMA_U[1] + y * (LGAMMA_U[2] + y * (LGAMMA_U[3] + y * (LGAMMA_U[4] + y * LGAMMA_U[5])))))
			p2 := f64(1) + y * (LGAMMA_V[1] + y * (LGAMMA_V[2] + y * (LGAMMA_V[3] + y * (LGAMMA_V[4] + y * LGAMMA_V[5]))))
			lgamma += (-0.5 * y + p1 / p2)
		}
	}
	else if x < 8 {
		// 2 <= x < 8
		i := int(x)
		y := x - f64(i)
		p := y * (LGAMMA_S[0] + y * (LGAMMA_S[1] + y * (LGAMMA_S[2] + y * (LGAMMA_S[3] + y * (LGAMMA_S[4] + y * (LGAMMA_S[5] + y * LGAMMA_S[6]))))))
		q := f64(1) + y * (LGAMMA_R[1] + y * (LGAMMA_R[2] + y * (LGAMMA_R[3] + y * (LGAMMA_R[4] + y * (LGAMMA_R[5] + y * LGAMMA_R[6])))))
		lgamma = 0.5 * y + p / q
		mut z := 1.0 // lgamma(1+s) = log(s) + lgamma(s)
		if i == 7 {
			z *= (y + 6)
			z *= (y + 5)
			z *= (y + 4)
			z *= (y + 3)
			z *= (y + 2)
			lgamma += log(z)
		}
		else if i == 6 {
			z *= (y + 5)
			z *= (y + 4)
			z *= (y + 3)
			z *= (y + 2)
			lgamma += log(z)
		}
		else if i == 5 {
			z *= (y + 4)
			z *= (y + 3)
			z *= (y + 2)
			lgamma += log(z)
		}
		else if i == 4 {
			z *= (y + 3)
			z *= (y + 2)
			lgamma += log(z)
		}
		else if i == 3 {
			z *= (y + 2)
			lgamma += log(z)
		}
	}
	else if x < two58 {
		// 8 <= x < 2**58
		t := math.log(x)
		z := f64(1) / x
		y := z * z
		w := LGAMMA_W[0] + z * (LGAMMA_W[1] + y * (LGAMMA_W[2] + y * (LGAMMA_W[3] + y * (LGAMMA_W[4] + y * (LGAMMA_W[5] + y * LGAMMA_W[6])))))
		lgamma = (x - 0.5) * (t - f64(1)) + w
	}
	else {
		// 2**58 <= x <= Inf
		lgamma = x * (log(x) - f64(1))
	}
	if neg {
		lgamma = nadj - lgamma
	}
	return lgamma,sign
}

// sin_pi(x) is a helper function for negative x
fn sin_pi(x_ f64) f64 {
	mut x := x_
	two52 := math.exp2(52) // 0x4330000000000000 ~4.5036e+15
	two53 := math.exp2(53) // 0x4340000000000000 ~9.0072e+15
	if x < 0.25 {
		return -sin(math.pi * x)
	}
	// argument reduction
	mut z := math.floor(x)
	mut n := 0
	if z != x {
		// inexact
		x = math.mod(x, 2)
		n = int(x * 4)
	}
	else {
		if x >= two53 {
			// x must be even
			x = 0
			n = 0
		}
		else {
			if x < two52 {
				z = x + two52 // exact
			}
			n = 1 & math.f64_bits(z)
			x = f64(n)
			n <<= 2
		}
	}
	if n == 0 {
		x = sin(math.pi * x)
	}
	else if n == 1 || n == 2 {
		x = cos(math.pi * (0.5 - x))
	}
	else if n == 3 || n == 4 {
		x = sin(math.pi * (f64(1) - x))
	}
	else if n == 5 || n == 6 {
		x = -cos(math.pi * (x - 1.5))
	}
	else {
		x = sin(math.pi * (x - 2))
	}
	return -x
}
