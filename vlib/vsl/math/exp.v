// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module math
#include <math.h>

fn C.exp(x f64) f64

import vsl.internal

const (
	f64_max_exp = f64(1024)
	f64_min_exp = f64(-1021)
	othreshold = 7.09782712893383973096e+02 // 0x40862E42FEFA39EF
	ln2_x56 = 3.88162421113569373274e+01 // 0x4043687a9f1af2b1
	ln2_halfX3 = 1.03972077083991796413e+00 // 0x3ff0a2b23f3bab73
	ln2_half = 3.46573590279972654709e-01 // 0x3fd62e42fefa39ef
	ln2Hi = 6.93147180369123816490e-01 // 0x3fe62e42fee00000
	ln2Lo = 1.90821492927058770002e-10 // 0x3dea39ef35793c76
	inv_ln2 = 1.44269504088896338700e+00 // 0x3ff71547652b82fe
	tiny = 1.0 / (1<<54) // 2**-54 = 0x3c90000000000000
	// scaled coefficients related to expm1
	EXPM1_Q1 = -3.33333333333331316428e-02 // 0xBFA11111111110F4
	EXPM1_Q2 = 1.58730158725481460165e-03 // 0x3F5A01A019FE5585
	EXPM1_Q3 = -7.93650757867487942473e-05 // 0xBF14CE199EAADBB7
	EXPM1_Q4 = 4.00821782732936239552e-06 // 0x3ED0CFCA86E65239
	EXPM1_Q5 = -2.01099218183624371326e-07 // 0xBE8AFDB76E09C32D
)

// exp returns e**x, the base-e exponential of x.
//
// special cases are:
//	exp(+inf) = +inf
//	exp(nan) = nan
// Very large values overflow to 0 or +inf.
// Very small values underflow to 1.
pub fn exp(x f64) f64 {
        return C.exp(x)

}

// exp2 returns 2**x, the base-2 exponential of x.
//
// special cases are the same as exp.
pub fn exp2(x f64) f64 {
        overflow  := 1.0239999999999999e+03
        underflow := -1.0740e+03

        p1 := 1.66666666666666657415e-01  /* 0x3FC55555; 0x55555555 */
        p2 := -2.77777777770155933842e-03 /* 0xBF66C16C; 0x16BEBD93 */
        p3 := 6.61375632143793436117e-05  /* 0x3F11566A; 0xAF25DE2C */
        p4 := -1.65339022054652515390e-06 /* 0xBEBBBD41; 0xC5D26BF1 */
        p5 := 4.13813679705723846039e-08  /* 0x3E663769; 0x72BEA4D0 */

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
	hi := t * ln2Hi
	lo := -t * ln2Lo
        
	r := hi - lo
	t = r * r
	c := r - t*(p1+t*(p2+t*(p3+t*(p4+t*p5))))
	y := f64(1) - ((lo - (r*c)/(f64(2)-c)) - hi)
	// TODO(rsc): make sure Ldexp can handle boundary k
	return ldexp(y, k)
}

pub fn ldexp(x f64, e int) f64 {
	if x == 0.0 {
		return x
	}
	else {
		mut y, ex := frexp(x)
		mut e2 := f64(e + ex)
		if e2 >= f64_max_exp {
			y *= pow(2.0, e2 - f64_max_exp + 1.0)
			e2 = f64_max_exp - 1.0
		}
		else if e2 <= f64_min_exp {
			y *= pow(2.0, e2 - f64_min_exp - 1.0)
			e2 = f64_min_exp + 1.0
		}
		p2 := pow(2.0, e2)
		return y * p2
	}
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
pub fn frexp(x f64) (f64, int) {
	if x == 0.0 {
                return f64(0), 0
        }
        else if !is_finite(x) {
                return x, 0
        }
        else if abs(x) >= 0.5 && abs(x) < 1 {    /* Handle the common case */
                return x, 0
        }
        else {
                ex := ceil(log(abs(x)) / ln2)
                mut ei := int(ex)

                /* Prevent underflow and overflow of 2**(-ei) */
                if ei < int(f64_min_exp) {
                        ei = int(f64_min_exp)
                }

                if ei > -int(f64_min_exp) {
                        ei = -int(f64_min_exp)
                }

                mut f := x * pow (2.0, -ei)

                if !is_finite(f) {
                        /* This should not happen */
                        return f, 0
                }

                for abs(f) >= 1.0 {
                        ei++
                        f /= 2.0
                }

                for abs(f) > 0 && abs(f) < 0.5 {
                        ei--
                        f *= 2.0
                }

                return f, ei
        }
}

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

        /* FIXME: this should be improved */

        if abs(x) < ln2 {
                /* Compute the taylor series S = x + (1/2!) x^2 + (1/3!) x^3 + ... */

                mut i := 1.0
                mut sum := x
                mut term := x / 1.0

                i++ 
                term *= x/f64(i)
                sum += term

                for abs(term) > abs(sum) * internal.f64_epsilon {
                        i++ 
                        term *= x/f64(i)
                        sum += term
                }
                
                return sum
        }
        else {
                return exp(x) - 1
        }
}
