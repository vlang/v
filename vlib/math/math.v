// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

#include <math.h>

fn C.acos(x f64) f64
fn C.asin(x f64) f64
fn C.atan(x f64) f64
fn C.atan2(y f64, x f64) f64
fn C.cbrt(x f64) f64
fn C.ceil(x f64) f64
fn C.cos(x f64) f64
fn C.cosf(x f32) f32
fn C.cosh(x f64) f64
fn C.erf(x f64) f64
fn C.erfc(x f64) f64
fn C.exp(x f64) f64
fn C.exp2(x f64) f64
fn C.fabs(x f64) f64
fn C.floor(x f64) f64
fn C.fmod(x f64, y f64) f64
fn C.hypot(x f64, y f64) f64
fn C.log(x f64) f64
fn C.log2(x f64) f64
fn C.log10(x f64) f64
fn C.lgamma(x f64) f64
fn C.pow(x f64, y f64) f64
fn C.powf(x f32, y f32) f32
fn C.round(x f64) f64
fn C.sin(x f64) f64
fn C.sinf(x f32) f32
fn C.sinh(x f64) f64
fn C.sqrt(x f64) f64
fn C.sqrtf(x f32) f32
fn C.tgamma(x f64) f64
fn C.tan(x f64) f64
fn C.tanf(x f32) f32
fn C.tanh(x f64) f64
fn C.trunc(x f64) f64

// NOTE
// When adding a new function, please make sure it's in the right place.
// All functions are sorted alphabetically.

// Returns the absolute value.
pub fn abs(a f64) f64 {
	return C.fabs(a)
}

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

// ceil returns the nearest f64 greater or equal to the provided value.
pub fn ceil(a f64) f64 {
	return C.ceil(a)
}

// cos calculates cosine.
pub fn cos(a f64) f64 {
	return C.cos(a)
}

// cosf calculates cosine. (float32)
pub fn cosf(a f32) f32 {
	return C.cosf(a)
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
	if base < 2 {
		panic('digits: Cannot find digits of n with base $base')
	}
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

// floor returns the nearest f64 lower or equal of the provided value.
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

// powf returns base raised to the provided power. (float32)
pub fn powf(a, b f32) f32 {
	return C.powf(a, b)
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

// sinf calculates sine. (float32)
pub fn sinf(a f32) f32 {
	return C.sinf(a)
}

// sinh calculates hyperbolic sine.
pub fn sinh(a f64) f64 {
	return C.sinh(a)
}

// sqrt calculates square-root of the provided value.
pub fn sqrt(a f64) f64 {
	return C.sqrt(a)
}

// sqrtf calculates square-root of the provided value. (float32)
pub fn sqrtf(a f32) f32 {
	return C.sqrtf(a)
}

// tan calculates tangent.
pub fn tan(a f64) f64 {
	return C.tan(a)
}

// tanf calculates tangent. (float32)
pub fn tanf(a f32) f32 {
	return C.tanf(a)
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

// Faster approximate sin() and cos() implemented from lolremez
pub fn aprox_sin(a f64) f64 {
	a0 := 1.91059300966915117e-31
	a1 := 1.00086760103908896
	a2 := -1.21276126894734565e-2
	a3 := -1.38078780785773762e-1
	a4 := -2.67353392911981221e-2
	a5 := 2.08026600266304389e-2
	a6 := -3.03996055049204407e-3
	a7 := 1.38235642404333740e-4
	return a0 + a * (a1 + a * (a2 + a * (a3 + a * (a4 + a * (a5 + a * (a6 + a * a7))))))
}

pub fn aprox_cos(a f64) f64 {
	a0 := 9.9995999154986614e-1
	a1 := 1.2548995793001028e-3
	a2 := -5.0648546280678015e-1
	a3 := 1.2942246466519995e-2
	a4 := 2.8668384702547972e-2
	a5 := 7.3726485210586547e-3
	a6 := -3.8510875386947414e-3
	a7 := 4.7196604604366623e-4
	a8 := -1.8776444013090451e-5
	return a0 + a * (a1 + a * (a2 + a * (a3 + a * (a4 + a * (a5 + a * (a6 + a * (a7 + a * a8)))))))
}