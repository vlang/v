// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

#include <float.h>

pub fn (d f64) str() string {
	buf := malloc(sizeof(double) * 5 + 1)// TODO
	C.sprintf(*char(buf), '%f', d)
	return tos(buf, vstrlen(buf))
}

pub fn (d f32) str() string {
	buf := malloc(sizeof(double) * 5 + 1)// TODO
	C.sprintf(*char(buf), '%f', d)
	return tos(buf, vstrlen(buf))
}

fn f32_abs(a f32) f32 {	return if a < 0 { -a } else { a } }
fn f64_abs(a f64) f64 {	return if a < 0 { -a } else { a } }

// compare floats using C epsilon
// ==
pub fn (a f64) eq(b f64) bool {
	return f64_abs(a - b) <= C.DBL_EPSILON	
}
pub fn (a f32) eq(b f32) bool {
	return f32_abs(a - b) <= C.FLT_EPSILON	
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




