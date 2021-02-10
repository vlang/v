// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module builtin

import strconv

#include <float.h>
/*
-----------------------------------
----- f64 to string functions -----
*/
// str return a `f64` as `string` in suitable notation.
[inline]
pub fn (x f64) str() string {
	abs_x := f64_abs(x)
	if abs_x >= 0.0001 && abs_x < 1.0e6 {
		return strconv.f64_to_str_l(x)
	} else {
		return strconv.ftoa_64(x)
	}
}

// str returns the value of the `float_literal` as a `string`.
[inline]
pub fn (d float_literal) str() string {
	return f64(d).str()
}

// strsci returns the `f64` as a `string` in scientific notation with `digit_num` decimals displayed, max 17 digits.
// Example: assert f64(1.234).strsci(3) == '1.234e+00'
[inline]
pub fn (x f64) strsci(digit_num int) string {
	mut n_digit := digit_num
	if n_digit < 1 {
		n_digit = 1
	} else if n_digit > 17 {
		n_digit = 17
	}
	return strconv.f64_to_str(x, n_digit)
}

// strlong returns a decimal notation of the `f64` as a `string`.
// Example: assert f64(1.23456).strlong() == '1.23456'
[inline]
pub fn (x f64) strlong() string {
	return strconv.f64_to_str_l(x)
}

/*
-----------------------------------
----- f32 to string functions -----
*/
// str returns a `f32` as `string` in suitable notation.
[inline]
pub fn (x f32) str() string {
	abs_x := f32_abs(x)
	if abs_x >= 0.0001 && abs_x < 1.0e6 {
		return strconv.f32_to_str_l(x)
	} else {
		return strconv.ftoa_32(x)
	}
}

// strsci returns the `f32` as a `string` in scientific notation with `digit_num` deciamals displayed, max 8 digits.
// Example: assert f32(1.234).strsci(3) == '1.234e+00'
[inline]
pub fn (x f32) strsci(digit_num int) string {
	mut n_digit := digit_num
	if n_digit < 1 {
		n_digit = 1
	} else if n_digit > 8 {
		n_digit = 8
	}
	return strconv.f32_to_str(x, n_digit)
}

// strlong returns a decimal notation of the `f32` as a `string`.
[inline]
pub fn (x f32) strlong() string {
	return strconv.f32_to_str_l(x)
}

/*
-----------------------
----- C functions -----
*/
// f32_abs returns the absolute value of `a` as a `f32` value.
// Example: assert f32_abs(-2.0) == 2.0
[inline]
pub fn f32_abs(a f32) f32 {
	return if a < 0 {
		-a
	} else {
		a
	}
}

// f64_abs returns the absolute value of `a` as a `f64` value.
// Example: assert f64_abs(-2.0) == f64(2.0)
[inline]
fn f64_abs(a f64) f64 {
	return if a < 0 {
		-a
	} else {
		a
	}
}

// f32_max returns the largest `f32` of input `a` and `b`.
// Example: assert f32_max(2.0,3.0) == 3.0
[inline]
pub fn f32_max(a f32, b f32) f32 {
	return if a > b {
		a
	} else {
		b
	}
}

// f32_min returns the smallest `f32` of input `a` and `b`.
// Example: assert f32_min(2.0,3.0) == 2.0
[inline]
pub fn f32_min(a f32, b f32) f32 {
	return if a < b {
		a
	} else {
		b
	}
}

// f64_max returns the largest `f64` of input `a` and `b`.
// Example: assert f64_max(2.0,3.0) == 3.0
[inline]
pub fn f64_max(a f64, b f64) f64 {
	return if a > b {
		a
	} else {
		b
	}
}

// f64_min returns the smallest `f64` of input `a` and `b`.
// Example: assert f64_min(2.0,3.0) == 2.0
[inline]
fn f64_min(a f64, b f64) f64 {
	return if a < b {
		a
	} else {
		b
	}
}

// eq_epsilon returns true if the `f32` is equal to input `b`.
// using an epsilon of typically 1E-5 or higher (backend/compiler dependent).
// Example: assert f32(2.0).eq_epsilon(2.0)
[inline]
pub fn (a f32) eq_epsilon(b f32) bool {
	hi := f32_max(f32_abs(a), f32_abs(b))
	delta := f32_abs(a - b)
	if hi > f32(1.0) {
		return delta <= hi * (4 * f32(C.FLT_EPSILON))
	} else {
		return (1 / (4 * f32(C.FLT_EPSILON))) * delta <= hi
	}
}

// eq_epsilon returns true if the `f64` is equal to input `b`.
// using an epsilon of typically 1E-9 or higher (backend/compiler dependent).
// Example: assert f64(2.0).eq_epsilon(2.0)
[inline]
pub fn (a f64) eq_epsilon(b f64) bool {
	hi := f64_max(f64_abs(a), f64_abs(b))
	delta := f64_abs(a - b)
	if hi > 1.0 {
		return delta <= hi * (4 * f64(C.DBL_EPSILON))
	} else {
		return (1 / (4 * f64(C.DBL_EPSILON))) * delta <= hi
	}
}
