// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

// TODO : The commented out functions need either a native V implementation, a
// JS specific implementation, or use some other JS math library, such as
// https://github.com/josdejong/mathjs
fn JS.Math.abs(x f64) f64 // Replaces C.fabs
fn JS.Math.acos(x f64) f64
fn JS.Math.asin(x f64) f64
fn JS.Math.atan(x f64) f64
fn JS.Math.atan2(y f64, x f64) f64
fn JS.Math.cbrt(x f64) f64
fn JS.Math.ceil(x f64) f64
fn JS.Math.cos(x f64) f64
fn JS.Math.cosh(x f64) f64
//fn JS.Math.erf(x f64) f64 // Not in standard JS Math object
//fn JS.Math.erfc(x f64) f64 // Not in standard JS Math object
fn JS.Math.exp(x f64) f64
//fn JS.Math.exp2(x f64) f64 // Not in standard JS Math object
fn JS.Math.floor(x f64) f64
//fn JS.Math.fmod(x f64, y f64) f64 // Not in standard JS Math object
//fn JS.Math.hypot(x f64, y f64) f64 // Not in standard JS Math object
fn JS.Math.log(x f64) f64
//fn JS.Math.log2(x f64) f64 // Not in standard JS Math object
//fn JS.Math.log10(x f64) f64 // Not in standard JS Math object
//fn JS.Math.lgamma(x f64) f64 // Not in standard JS Math object
fn JS.Math.pow(x f64, y f64) f64
fn JS.Math.round(x f64) f64
fn JS.Math.sin(x f64) f64
fn JS.Math.sinh(x f64) f64
fn JS.Math.sqrt(x f64) f64
//fn JS.Math.tgamma(x f64) f64 // Not in standard JS Math object
fn JS.Math.tan(x f64) f64
fn JS.Math.tanh(x f64) f64
fn JS.Math.trunc(x f64) f64

// NOTE
// When adding a new function, please make sure it's in the right place.
// All functions are sorted alphabetically.

// Returns the absolute value.
[inline]
pub fn abs(a f64) f64 {
	return JS.Math.abs(a)
}

// acos calculates inverse cosine (arccosine).
[inline]
pub fn acos(a f64) f64 {
	return JS.Math.acos(a)
}

// asin calculates inverse sine (arcsine).
[inline]
pub fn asin(a f64) f64 {
	return JS.Math.asin(a)
}

// atan calculates inverse tangent (arctangent).
[inline]
pub fn atan(a f64) f64 {
	return JS.Math.atan(a)
}

// atan2 calculates inverse tangent with two arguments, returns the angle between the X axis and the point.
[inline]
pub fn atan2(a, b f64) f64 {
	return JS.Math.atan2(a, b)
}

// cbrt calculates cubic root.
[inline]
pub fn cbrt(a f64) f64 {
	return JS.Math.cbrt(a)
}

// ceil returns the nearest f64 greater or equal to the provided value.
[inline]
pub fn ceil(a f64) f64 {
	return JS.Math.ceil(a)
}

// cos calculates cosine.
[inline]
pub fn cos(a f64) f64 {
	return JS.Math.cos(a)
}

// cosf calculates cosine. (float32). This doesn't exist in JS
[inline]
pub fn cosf(a f32) f32 {
	return f32(JS.Math.cos(a))
}

// cosh calculates hyperbolic cosine.
[inline]
pub fn cosh(a f64) f64 {
	return JS.Math.cosh(a)
}

// exp calculates exponent of the number (math.pow(math.E, a)).
[inline]
pub fn exp(a f64) f64 {
	return JS.Math.exp(a)
}

// erf computes the error function value
[inline]
pub fn erf(a f64) f64 {
	return JS.Math.erf(a)
}

// erfc computes the complementary error function value
[inline]
pub fn erfc(a f64) f64 {
	return JS.Math.erfc(a)
}

// exp2 returns the base-2 exponential function of a (math.pow(2, a)).
[inline]
pub fn exp2(a f64) f64 {
	return JS.Math.exp2(a)
}

// floor returns the nearest f64 lower or equal of the provided value.
[inline]
pub fn floor(a f64) f64 {
	return JS.Math.floor(a)
}

// fmod returns the floating-point remainder of number / denom (rounded towards zero):
[inline]
pub fn fmod(a, b f64) f64 {
	return JS.Math.fmod(a, b)
}

// gamma computes the gamma function value
[inline]
pub fn gamma(a f64) f64 {
	return JS.Math.tgamma(a)
}

// Returns hypotenuse of a right triangle.
[inline]
pub fn hypot(a, b f64) f64 {
	return JS.Math.hypot(a, b)
}

// log calculates natural (base-e) logarithm of the provided value.
[inline]
pub fn log(a f64) f64 {
	return JS.Math.log(a)
}

// log2 calculates base-2 logarithm of the provided value.
[inline]
pub fn log2(a f64) f64 {
	return JS.Math.log2(a)
}

// log10 calculates the common (base-10) logarithm of the provided value.
[inline]
pub fn log10(a f64) f64 {
	return JS.Math.log10(a)
}

// log_gamma computes the log-gamma function value
[inline]
pub fn log_gamma(a f64) f64 {
	return JS.Math.lgamma(a)
}

// log_n calculates base-N logarithm of the provided value.
[inline]
pub fn log_n(a, b f64) f64 {
	return JS.Math.log(a) / JS.Math.log(b)
}

// pow returns base raised to the provided power.
[inline]
pub fn pow(a, b f64) f64 {
	return JS.Math.pow(a, b)
}

// powf returns base raised to the provided power. (float32)
[inline]
pub fn powf(a, b f32) f32 {
	return f32(JS.Math.pow(a, b))
}

// round returns the integer nearest to the provided value.
[inline]
pub fn round(f f64) f64 {
	return JS.Math.round(f)
}

// sin calculates sine.
[inline]
pub fn sin(a f64) f64 {
	return JS.Math.sin(a)
}

// sinf calculates sine. (float32)
[inline]
pub fn sinf(a f32) f32 {
	return f32(JS.Math.sin(a))
}

// sinh calculates hyperbolic sine.
[inline]
pub fn sinh(a f64) f64 {
	return JS.Math.sinh(a)
}

// sqrt calculates square-root of the provided value.
[inline]
pub fn sqrt(a f64) f64 {
	return JS.Math.sqrt(a)
}

// sqrtf calculates square-root of the provided value. (float32)
[inline]
pub fn sqrtf(a f32) f32 {
	return f32(JS.Math.sqrt(a))
}

// tan calculates tangent.
[inline]
pub fn tan(a f64) f64 {
	return JS.Math.tan(a)
}

// tanf calculates tangent. (float32)
[inline]
pub fn tanf(a f32) f32 {
	return f32(JS.Math.tan(a))
}

// tanh calculates hyperbolic tangent.
[inline]
pub fn tanh(a f64) f64 {
	return JS.Math.tanh(a)
}

// trunc rounds a toward zero, returning the nearest integral value that is not
// larger in magnitude than a.
[inline]
pub fn trunc(a f64) f64 {
	return JS.Math.trunc(a)
}
