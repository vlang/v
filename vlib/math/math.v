// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module math

#include <math.h>

// NOTE
// When adding a new function, please make sure it's in the right place.
// All functions are sorted alphabetically.

// Returns the absolute value.
pub fn abs(a f64) f64 {
	if a < 0 {
		return -a
	}
	return a
}

fn C.acos(a f64) f64

// acos calculates inverse cosine (arccosine).
pub fn acos(a f64) f64 {
	return C.acos(a)
}

// asin calculates inverse sine (arcsine).
pub fn asin(a f64) f64 {
	return C.asin(a)
}

// atan calculates inverse tangent (arctangent).
pub fn atan(a f64) f64 {
	return C.atan(a)
}

// atan2 calculates inverse tangent with two arguments, returns the angle between the X axis and the point.
pub fn atan2(a, b f64) f64 {
	return C.atan2(a, b)
}

// cbrt calculates cubic root.
pub fn cbrt(a f64) f64 {
	return C.cbrt(a)
}

// ceil returns the nearest integer greater or equal to the provided value.
pub fn ceil(a f64) int {
	return C.ceil(a)
}

// cos calculates cosine.
pub fn cos(a f64) f64 {
	return C.cos(a)
}

// cosh calculates hyperbolic cosine.
pub fn cosh(a f64) f64 {
	return C.cosh(a)
}

// degrees convert from degrees to radians.
pub fn degrees(radians f64) f64 {
	return radians * (180.0 / pi)
}

// exp calculates exponent of the number (math.pow(math.E, a)).
pub fn exp(a f64) f64 {
	return C.exp(a)
}

// digits returns an array of the digits of n in the given base.
pub fn digits(_n, base int) []int {
	mut n := _n
	mut sign := 1
	if n < 0 {
		sign = -1
		n = -n
	}
	mut res := []int
	for n != 0 {
		res << (n % base) * sign
		n /= base
	}
	return res
}

// erf computes the error function value
pub fn erf(a f64) f64 {
	return C.erf(a)
}

// erfc computes the complementary error function value
pub fn erfc(a f64) f64 {
	return C.erfc(a)
}

// exp2 returns the base-2 exponential function of a (math.pow(2, a)).
pub fn exp2(a f64) f64 {
	return C.exp2(a)
}

// factorial calculates the factorial of the provided value.
// TODO bring back once multiple value functions are implemented
/*
fn recursive_product( n int, current_number_ptr &int) int{
    mut m := n / 2
    if (m == 0){
        return *current_number_ptr += 2
    }
    if (n == 2){
        return (*current_number_ptr += 2) * (*current_number_ptr += 2)
    }
    return recursive_product((n - m), *current_number_ptr) * recursive_product(m, *current_number_ptr)
}

pub fn factorial(n int) i64 {
    if n < 0 {
        panic('factorial: Cannot find factorial of negative number')
    }
    if n < 2 {
        return i64(1)
    }
    mut r := 1
    mut p := 1
    mut current_number := 1
    mut h := 0
    mut shift := 0
    mut high := 1
    mut len := high
    mut log2n := int(floor(log2(n)))
    for ;h != n; {
        shift += h
        h = n >> log2n
        log2n -= 1
        len = high
        high = (h - 1) | 1
        len = (high - len)/2
        if (len > 0){
            p *= recursive_product(len, &current_number)
            r *= p
        }
    }
    return i64((r << shift))
}
*/

// floor returns the nearest integer lower or equal of the provided value.
pub fn floor(a f64) f64 {
	return C.floor(a)
}

// fmod returns the floating-point remainder of number / denom (rounded towards zero):
pub fn fmod(a, b f64) f64 {
	return C.fmod(a, b)
}

// gamma computes the gamma function value
pub fn gamma(a f64) f64 {
	return C.tgamma(a)
}

// gcd calculates greatest common (positive) divisor (or zero if a and b are both zero).
pub fn gcd(a_, b_ i64) i64 {
	mut a := a_
	mut b := b_
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a %= b
		if a == 0 {
			return b
		}
		b %= a
	}
	return a
}

// Returns hypotenuse of a right triangle.
pub fn hypot(a, b f64) f64 {
	return C.hypot(a, b)
}

// lcm calculates least common (non-negative) multiple.
pub fn lcm(a, b i64) i64 {
	if a == 0 {
		return a
	}
	res := a * (b / gcd(b, a))
	if res < 0 {
		return -res
	}
	return res
}

// log calculates natural (base-e) logarithm of the provided value.
pub fn log(a f64) f64 {
	return C.log(a)
}

// log2 calculates base-2 logarithm of the provided value.
pub fn log2(a f64) f64 {
	return C.log2(a)
}

// log10 calculates the common (base-10) logarithm of the provided value.
pub fn log10(a f64) f64 {
	return C.log10(a)
}

// log_gamma computes the log-gamma function value
pub fn log_gamma(a f64) f64 {
	return C.lgamma(a)
}

// log_n calculates base-N logarithm of the provided value.
pub fn log_n(a, b f64) f64 {
	return C.log(a) / C.log(b)
}

// max returns the maximum value of the two provided.
pub fn max(a, b f64) f64 {
	if a > b {
		return a
	}
	return b
}

// min returns the minimum value of the two provided.
pub fn min(a, b f64) f64 {
	if a < b {
		return a
	}
	return b
}

// pow returns base raised to the provided power.
pub fn pow(a, b f64) f64 {
	return C.pow(a, b)
}

// radians convert from radians to degrees.
pub fn radians(degrees f64) f64 {
	return degrees * (pi / 180.0)
}

// round returns the integer nearest to the provided value.
pub fn round(f f64) f64 {
	return C.round(f)
}

// sin calculates sine.
pub fn sin(a f64) f64 {
	return C.sin(a)
}

// sinh calculates hyperbolic sine.
pub fn sinh(a f64) f64 {
	return C.sinh(a)
}

// sqrt calculates square-root of the provided value.
pub fn sqrt(a f64) f64 {
	return C.sqrt(a)
}
// tan calculates tangent.
pub fn tan(a f64) f64 {
	return C.tan(a)
}

// tanh calculates hyperbolic tangent.
pub fn tanh(a f64) f64 {
	return C.tanh(a)
}

// trunc rounds a toward zero, returning the nearest integral value that is not
// larger in magnitude than a.
pub fn trunc(a f64) f64 {
	return C.trunc(a)
}
