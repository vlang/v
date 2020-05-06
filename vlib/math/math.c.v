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
[inline]
fn sys_abs(a f64) f64 {
	return C.fabs(a)
}

// sys_acos calculates inverse cosine (arccosine).
[inline]
fn sys_acos(a f64) f64 {
	return C.acos(a)
}

// sys_asin calculates inverse sine (arcsine).
[inline]
fn sys_asin(a f64) f64 {
	return C.asin(a)
}

// sys_atan calculates inverse tangent (arctangent).
[inline]
fn sys_atan(a f64) f64 {
	return C.atan(a)
}

// sys_atan2 calculates inverse tangent with two arguments, returns the angle between the X axis and the point.
[inline]
fn sys_atan2(a, b f64) f64 {
	return C.atan2(a, b)
}

// sys_cbrt calculates cubic root.
[inline]
fn sys_cbrt(a f64) f64 {
	return C.cbrt(a)
}

// sys_ceil returns the nearest f64 greater or equal to the provided value.
[inline]
fn sys_ceil(a f64) f64 {
	return C.ceil(a)
}

// sys_cos calculates cosine.
[inline]
fn sys_cos(a f64) f64 {
	return C.cos(a)
}

// sys_cosf calculates cosine. (float32)
[inline]
fn sys_cosf(a f32) f32 {
	return C.cosf(a)
}

// sys_cosh calculates hyperbolic cosine.
[inline]
fn sys_cosh(a f64) f64 {
	return C.cosh(a)
}

// sys_exp calculates exponent of the number (math.pow(math.E, a)).
[inline]
fn sys_exp(a f64) f64 {
	return C.exp(a)
}

// sys_erf computes the error function value
[inline]
fn sys_erf(a f64) f64 {
	return C.erf(a)
}

// sys_erfc computes the complementary error function value
[inline]
fn sys_erfc(a f64) f64 {
	return C.erfc(a)
}

// sys_exp2 returns the base-2 exponential function of a (math.pow(2, a)).
[inline]
fn sys_exp2(a f64) f64 {
	return C.exp2(a)
}

// sys_floor returns the nearest f64 lower or equal of the provided value.
[inline]
fn sys_floor(a f64) f64 {
	return C.floor(a)
}

// sys_fmod returns the floating-point remainder of number / denom (rounded towards zero):
[inline]
fn sys_fmod(a, b f64) f64 {
	return C.fmod(a, b)
}

// sys_gamma computes the gamma function value
[inline]
fn sys_tgamma(a f64) f64 {
	return C.tgamma(a)
}

// sys_Returns hypotenuse of a right triangle.
[inline]
fn sys_hypot(a, b f64) f64 {
	return C.hypot(a, b)
}

// sys_log calculates natural (base-e) logarithm of the provided value.
[inline]
fn sys_log(a f64) f64 {
	return C.log(a)
}

// sys_log2 calculates base-2 logarithm of the provided value.
[inline]
fn sys_log2(a f64) f64 {
	return C.log2(a)
}

// sys_log10 calculates the common (base-10) logarithm of the provided value.
[inline]
fn sys_log10(a f64) f64 {
	return C.log10(a)
}

// sys_log_gamma computes the log-gamma function value
[inline]
fn sys_log_gamma(a f64) f64 {
	return C.lgamma(a)
}

// sys_log_n calculates base-N logarithm of the provided value.
[inline]
fn sys_log_n(a, b f64) f64 {
	return C.log(a) / C.log(b)
}

// sys_pow returns base raised to the provided power.
[inline]
fn sys_pow(a, b f64) f64 {
	return C.pow(a, b)
}

// sys_powf returns base raised to the provided power. (float32)
[inline]
fn sys_powf(a, b f32) f32 {
	return C.powf(a, b)
}

// sys_round returns the integer nearest to the provided value.
[inline]
fn sys_round(f f64) f64 {
	return C.round(f)
}

// sys_sin calculates sine.
[inline]
fn sys_sin(a f64) f64 {
	return C.sin(a)
}

// sys_sinf calculates sine. (float32)
[inline]
fn sys_sinf(a f32) f32 {
	return C.sinf(a)
}

// sys_sinh calculates hyperbolic sine.
[inline]
fn sys_sinh(a f64) f64 {
	return C.sinh(a)
}

// sys_sqrt calculates square-root of the provided value.
[inline]
fn sys_sqrt(a f64) f64 {
	return C.sqrt(a)
}

// sys_sqrtf calculates square-root of the provided value. (float32)
[inline]
fn sys_sqrtf(a f32) f32 {
	return C.sqrtf(a)
}

// sys_tan calculates tangent.
[inline]
fn sys_tan(a f64) f64 {
	return C.tan(a)
}

// sys_tanf calculates tangent. (float32)
[inline]
fn sys_tanf(a f32) f32 {
	return C.tanf(a)
}

// sys_tanh calculates hyperbolic tangent.
[inline]
fn sys_tanh(a f64) f64 {
	return C.tanh(a)
}

// sys_trunc rounds a toward zero, returning the nearest integral value that is not
// larger in magnitude than a.
[inline]
fn sys_trunc(a f64) f64 {
	return C.trunc(a)
}
