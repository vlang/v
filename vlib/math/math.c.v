// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

#include <math.h>
$if windows {
	$if tinyc {
		#flag @VEXEROOT/thirdparty/tcc/lib/openlibm.o
	}
}
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
[inline]
pub fn abs(a f64) f64 {
	return C.fabs(a)
}

// acos calculates inverse cosine (arccosine).
[inline]
pub fn acos(a f64) f64 {
	return C.acos(a)
}

// asin calculates inverse sine (arcsine).
[inline]
pub fn asin(a f64) f64 {
	return C.asin(a)
}

// atan calculates inverse tangent (arctangent).
[inline]
pub fn atan(a f64) f64 {
	return C.atan(a)
}

// atan2 calculates inverse tangent with two arguments, returns the angle between the X axis and the point.
[inline]
pub fn atan2(a f64, b f64) f64 {
	return C.atan2(a, b)
}

// cbrt calculates cubic root.
[inline]
pub fn cbrt(a f64) f64 {
	return C.cbrt(a)
}

// ceil returns the nearest f64 greater or equal to the provided value.
[inline]
pub fn ceil(a f64) f64 {
	return C.ceil(a)
}

// cos calculates cosine.
[inline]
pub fn cos(a f64) f64 {
	return C.cos(a)
}

// cosf calculates cosine. (float32)
[inline]
pub fn cosf(a f32) f32 {
	return C.cosf(a)
}

// cosh calculates hyperbolic cosine.
[inline]
pub fn cosh(a f64) f64 {
	return C.cosh(a)
}

// exp calculates exponent of the number (math.pow(math.E, a)).
[inline]
pub fn exp(a f64) f64 {
	return C.exp(a)
}

// erf computes the error function value
[inline]
pub fn erf(a f64) f64 {
	return C.erf(a)
}

// erfc computes the complementary error function value
[inline]
pub fn erfc(a f64) f64 {
	return C.erfc(a)
}

// exp2 returns the base-2 exponential function of a (math.pow(2, a)).
[inline]
pub fn exp2(a f64) f64 {
	return C.exp2(a)
}

// floor returns the nearest f64 lower or equal of the provided value.
[inline]
pub fn floor(a f64) f64 {
	return C.floor(a)
}

// fmod returns the floating-point remainder of number / denom (rounded towards zero):
[inline]
pub fn fmod(a f64, b f64) f64 {
	return C.fmod(a, b)
}

// gamma computes the gamma function value
[inline]
pub fn gamma(a f64) f64 {
	return C.tgamma(a)
}

// Returns hypotenuse of a right triangle.
[inline]
pub fn hypot(a f64, b f64) f64 {
	return C.hypot(a, b)
}

// log calculates natural (base-e) logarithm of the provided value.
[inline]
pub fn log(a f64) f64 {
	return C.log(a)
}

// log2 calculates base-2 logarithm of the provided value.
[inline]
pub fn log2(a f64) f64 {
	return C.log2(a)
}

// log10 calculates the common (base-10) logarithm of the provided value.
[inline]
pub fn log10(a f64) f64 {
	return C.log10(a)
}

// log_gamma computes the log-gamma function value
[inline]
pub fn log_gamma(a f64) f64 {
	return C.lgamma(a)
}

// log_n calculates base-N logarithm of the provided value.
[inline]
pub fn log_n(a f64, b f64) f64 {
	return C.log(a) / C.log(b)
}

// pow returns base raised to the provided power.
[inline]
pub fn pow(a f64, b f64) f64 {
	return C.pow(a, b)
}

// powf returns base raised to the provided power. (float32)
[inline]
pub fn powf(a f32, b f32) f32 {
	return C.powf(a, b)
}

// round returns the integer nearest to the provided value.
[inline]
pub fn round(f f64) f64 {
	return C.round(f)
}

// sin calculates sine.
[inline]
pub fn sin(a f64) f64 {
	return C.sin(a)
}

// sinf calculates sine. (float32)
[inline]
pub fn sinf(a f32) f32 {
	return C.sinf(a)
}

// sinh calculates hyperbolic sine.
[inline]
pub fn sinh(a f64) f64 {
	return C.sinh(a)
}

// sqrt calculates square-root of the provided value.
[inline]
pub fn sqrt(a f64) f64 {
	return C.sqrt(a)
}

// sqrtf calculates square-root of the provided value. (float32)
[inline]
pub fn sqrtf(a f32) f32 {
	return C.sqrtf(a)
}

// tan calculates tangent.
[inline]
pub fn tan(a f64) f64 {
	return C.tan(a)
}

// tanf calculates tangent. (float32)
[inline]
pub fn tanf(a f32) f32 {
	return C.tanf(a)
}

// tanh calculates hyperbolic tangent.
[inline]
pub fn tanh(a f64) f64 {
	return C.tanh(a)
}

// trunc rounds a toward zero, returning the nearest integral value that is not
// larger in magnitude than a.
[inline]
pub fn trunc(a f64) f64 {
	return C.trunc(a)
}
