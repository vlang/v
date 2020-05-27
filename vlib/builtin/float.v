// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

import strconv.ftoa

#include <float.h>
// ----- f64 to string functions -----
// str return a f64 as string in scientific notation, auto display digits limit
[inline]
pub fn (d f64) str() string {
	return ftoa.ftoa_64(d)
}

[inline]
pub fn (d any_float) str() string {
	x := f64(d)
	abs_x := f64_abs(x)
	if abs_x >= 0.01 && abs_x < 1.0e16 {
		return ftoa.f64_to_str_l(x)
	} else {
		return ftoa.ftoa_64(x)
	}
}

// return a string of the input f64 in scientific notation with digit_num deciamals displayed, max 17 digits
[inline]
pub fn (x f64) strsci(digit_num int) string {
	mut n_digit := digit_num
	if n_digit < 1 {
		n_digit = 1
	} else if n_digit > 17 {
		n_digit = 17
	}
	return ftoa.f64_to_str(x, n_digit)
}

// return a decimal notation of the input f64
[inline]
pub fn (x f64) strlong() string {
	return ftoa.f64_to_str_l(x)
}

// ----- f32 to string functions -----
// str return a f32 as string in scientific notation, auto display digits limit
[inline]
pub fn (d f32) str() string {
	return ftoa.ftoa_32(d)
}

// return a string of the input f32 in scientific notation with digit_num deciamals displayed, max 8 digits
[inline]
pub fn (x f32) strsci(digit_num int) string {
	mut n_digit := digit_num
	if n_digit < 1 {
		n_digit = 1
	} else if n_digit > 8 {
		n_digit = 8
	}
	return ftoa.f32_to_str(x, n_digit)
}

// return a decimal notation of the input f32
[inline]
pub fn (x f32) strlong() string {
	return ftoa.f32_to_str_l(x)
}

// ----- C functions -----
[inline]
fn f32_abs(a f32) f32 {
	return if a < 0 {
		-a
	} else {
		a
	}
}

[inline]
fn f64_abs(a f64) f64 {
	return if a < 0 {
		-a
	} else {
		a
	}
}

// compare floats using C epsilon
// ==
[inline]
pub fn (a f64) eq(b f64) bool {
	return f64_abs(a - b) <= C.DBL_EPSILON
}

[inline]
pub fn (a f32) eq(b f32) bool {
	return f32_abs(a - b) <= f32(C.FLT_EPSILON)
}

pub fn (a f64) eqbit(b f64) bool {
	return C.DEFAULT_EQUAL(a, b)
}

pub fn (a f32) eqbit(b f32) bool {
	return C.DEFAULT_EQUAL(a, b)
}

// !=
fn (a f64) ne(b f64) bool {
	return !a.eq(b)
}

fn (a f32) ne(b f32) bool {
	return !a.eq(b)
}

pub fn (a f64) nebit(b f64) bool {
	return C.DEFAULT_NOT_EQUAL(a, b)
}

pub fn (a f32) nebit(b f32) bool {
	return C.DEFAULT_NOT_EQUAL(a, b)
}

// a < b
fn (a f64) lt(b f64) bool {
	return a.ne(b) && a.ltbit(b)
}

fn (a f32) lt(b f32) bool {
	return a.ne(b) && a.ltbit(b)
}

fn (a f64) ltbit(b f64) bool {
	return C.DEFAULT_LT(a, b)
}

fn (a f32) ltbit(b f32) bool {
	return C.DEFAULT_LT(a, b)
}

// a <= b
fn (a f64) le(b f64) bool {
	return !a.gt(b)
}

fn (a f32) le(b f32) bool {
	return !a.gt(b)
}

fn (a f64) lebit(b f64) bool {
	return C.DEFAULT_LE(a, b)
}

fn (a f32) lebit(b f32) bool {
	return C.DEFAULT_LE(a, b)
}

// a > b
fn (a f64) gt(b f64) bool {
	return a.ne(b) && a.gtbit(b)
}

fn (a f32) gt(b f32) bool {
	return a.ne(b) && a.gtbit(b)
}

fn (a f64) gtbit(b f64) bool {
	return C.DEFAULT_GT(a, b)
}

fn (a f32) gtbit(b f32) bool {
	return C.DEFAULT_GT(a, b)
}

// a >= b
fn (a f64) ge(b f64) bool {
	return !a.lt(b)
}

fn (a f32) ge(b f32) bool {
	return !a.lt(b)
}

fn (a f64) gebit(b f64) bool {
	return C.DEFAULT_GE(a, b)
}

fn (a f32) gebit(b f32) bool {
	return C.DEFAULT_GE(a, b)
}
