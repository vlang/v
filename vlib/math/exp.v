module math

import math.internal

const (
	f64_max_exp = f64(1024)
	f64_min_exp = f64(-1021)
	threshold   = 7.09782712893383973096e+02 // 0x40862E42FEFA39EF
	ln2_x56     = 3.88162421113569373274e+01 // 0x4043687a9f1af2b1
	ln2_halfx3  = 1.03972077083991796413e+00 // 0x3ff0a2b23f3bab73
	ln2_half    = 3.46573590279972654709e-01 // 0x3fd62e42fefa39ef
	ln2hi       = 6.93147180369123816490e-01 // 0x3fe62e42fee00000
	ln2lo       = 1.90821492927058770002e-10 // 0x3dea39ef35793c76
	inv_ln2     = 1.44269504088896338700e+00 // 0x3ff71547652b82fe
	// scaled coefficients related to expm1
	expm1_q1    = -3.33333333333331316428e-02 // 0xBFA11111111110F4
	expm1_q2    = 1.58730158725481460165e-03 // 0x3F5A01A019FE5585
	expm1_q3    = -7.93650757867487942473e-05 // 0xBF14CE199EAADBB7
	expm1_q4    = 4.00821782732936239552e-06 // 0x3ED0CFCA86E65239
	expm1_q5    = -2.01099218183624371326e-07 // 0xBE8AFDB76E09C32D
)

// exp returns e**x, the base-e exponential of x.
//
// special cases are:
// exp(+inf) = +inf
// exp(nan) = nan
// Very large values overflow to 0 or +inf.
// Very small values underflow to 1.
pub fn exp(x f64) f64 {
	log2e := 1.44269504088896338700e+00
	overflow := 7.09782712893383973096e+02
	underflow := -7.45133219101941108420e+02
	near_zero := 1.0 / (1 << 28) // 2**-28
	// special cases
	if is_nan(x) || is_inf(x, 1) {
		return x
	}
	if is_inf(x, -1) {
		return 0.0
	}
	if x > overflow {
		return inf(1)
	}
	if x < underflow {
		return 0.0
	}
	if -near_zero < x && x < near_zero {
		return 1.0 + x
	}
	// reduce; computed as r = hi - lo for extra precision.
	mut k := 0
	if x < 0 {
		k = int(log2e * x - 0.5)
	}
	if x > 0 {
		k = int(log2e * x + 0.5)
	}
	hi := x - f64(k) * math.ln2hi
	lo := f64(k) * math.ln2lo
	// compute
	return expmulti(hi, lo, k)
}

// exp2 returns 2**x, the base-2 exponential of x.
//
// special cases are the same as exp.
pub fn exp2(x f64) f64 {
	overflow := 1.0239999999999999e+03
	underflow := -1.0740e+03
	if is_nan(x) || is_inf(x, 1) {
		return x
	}
	if is_inf(x, -1) {
		return 0
	}
	if x > overflow {
		return inf(1)
	}
	if x < underflow {
		return 0
	}
	// argument reduction; x = r×lg(e) + k with |r| ≤ ln(2)/2.
	// computed as r = hi - lo for extra precision.
	mut k := 0
	if x > 0 {
		k = int(x + 0.5)
	}
	if x < 0 {
		k = int(x - 0.5)
	}
	mut t := x - f64(k)
	hi := t * math.ln2hi
	lo := -t * math.ln2lo
	// compute
	return expmulti(hi, lo, k)
}

// ldexp calculates frac*(2**exp)
pub fn ldexp(frac f64, exp int) f64 {
	return scalbn(frac, exp)
}

// frexp breaks f into a normalized fraction
// and an integral power of two.
// It returns frac and exp satisfying f == frac × 2**exp,
// with the absolute value of frac in the interval [½, 1).
//
// special cases are:
// frexp(±0) = ±0, 0
// frexp(±inf) = ±inf, 0
// frexp(nan) = nan, 0
// pub fn frexp(f f64) (f64, int) {
// mut y := f64_bits(x)
// ee := int((y >> 52) & 0x7ff)
// // special cases
// if ee == 0 {
//	if x != 0.0 {
// 	x1p64 := f64_from_bits(0x43f0000000000000)
// 	z,e_ := frexp(x * x1p64)
// 	return z,e_ - 64
// }
// return x,0
// } else if ee == 0x7ff {
// return x,0
// }
// e_ := ee - 0x3fe
// y &= 0x800fffffffffffff
// y |= 0x3fe0000000000000
// return f64_from_bits(y),e_
pub fn frexp(x f64) (f64, int) {
	mut y := f64_bits(x)
	ee := int((y >> 52) & 0x7ff)
	if ee == 0 {
		if x != 0.0 {
			x1p64 := f64_from_bits(u64(0x43f0000000000000))
			z, e_ := frexp(x * x1p64)
			return z, e_ - 64
		}
		return x, 0
	} else if ee == 0x7ff {
		return x, 0
	}
	e_ := ee - 0x3fe
	y &= u64(0x800fffffffffffff)
	y |= u64(0x3fe0000000000000)
	return f64_from_bits(y), e_
}

// expm1 calculates e**x - 1
// special cases are:
// expm1(+inf) = +inf
// expm1(-inf) = -1
// expm1(nan) = nan
pub fn expm1(x f64) f64 {
	if is_inf(x, 1) || is_nan(x) {
		return x
	}
	if is_inf(x, -1) {
		return f64(-1)
	}
	// FIXME: this should be improved
	if abs(x) < ln2 { // Compute the taylor series S = x + (1/2!) x^2 + (1/3!) x^3 + ...
		mut i := 1.0
		mut sum := x
		mut term := x / 1.0
		i++
		term *= x / f64(i)
		sum += term
		for abs(term) > abs(sum) * internal.f64_epsilon {
			i++
			term *= x / f64(i)
			sum += term
		}
		return sum
	} else {
		return exp(x) - 1
	}
}

fn expmulti(hi f64, lo f64, k int) f64 {
	exp_p1 := 1.66666666666666657415e-01 // 0x3FC55555; 0x55555555
	exp_p2 := -2.77777777770155933842e-03 // 0xBF66C16C; 0x16BEBD93
	exp_p3 := 6.61375632143793436117e-05 // 0x3F11566A; 0xAF25DE2C
	exp_p4 := -1.65339022054652515390e-06 // 0xBEBBBD41; 0xC5D26BF1
	exp_p5 := 4.13813679705723846039e-08 // 0x3E663769; 0x72BEA4D0
	r := hi - lo
	t := r * r
	c := r - t * (exp_p1 + t * (exp_p2 + t * (exp_p3 + t * (exp_p4 + t * exp_p5))))
	y := 1 - ((lo - (r * c) / (2 - c)) - hi)
	// TODO(rsc): make sure ldexp can handle boundary k
	return ldexp(y, k)
}
