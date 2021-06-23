// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

// imin returns the smallest of two integer values
[inline]
pub fn imin(a int, b int) int {
	return if a < b { a } else { b }
}

// imin returns the biggest of two integer values
[inline]
pub fn imax(a int, b int) int {
	return if a > b { a } else { b }
}

// iabs returns an integer as absolute value
[inline]
pub fn iabs(v int) int {
	return if v > 0 { v } else { -v }
}

// umin returns the smallest of two u32 values
[inline]
pub fn umin(a u32, b u32) u32 {
	return if a < b { a } else { b }
}

// umax returns the biggest of two u32 values
[inline]
pub fn umax(a u32, b u32) u32 {
	return if a > b { a } else { b }
}

// uabs returns an u32 as absolute value
[inline]
pub fn uabs(v u32) u32 {
	return if v > 0 { v } else { -v }
}

// fmin_32 returns the smallest `f32` of input `a` and `b`.
// Example: assert fmin_32(2.0,3.0) == 2.0
[inline]
pub fn fmin_32(a f32, b f32) f32 {
	return if a < b { a } else { b }
}

// fmax_32 returns the largest `f32` of input `a` and `b`.
// Example: assert fmax_32(2.0,3.0) == 3.0
[inline]
pub fn fmax_32(a f32, b f32) f32 {
	return if a > b { a } else { b }
}

// fabs_32 returns the absolute value of `a` as a `f32` value.
// Example: assert fabs_32(-2.0) == 2.0
[inline]
pub fn fabs_32(v f32) f32 {
	return if v > 0 { v } else { -v }
}

// fmin_64 returns the smallest `f64` of input `a` and `b`.
// Example: assert fmin_64(2.0,3.0) == 2.0
[inline]
pub fn fmin_64(a f64, b f64) f64 {
	return if a < b { a } else { b }
}

// fmax_64 returns the largest `f64` of input `a` and `b`.
// Example: assert fmax_64(2.0,3.0) == 3.0
[inline]
pub fn fmax_64(a f64, b f64) f64 {
	return if a > b { a } else { b }
}

// fabs_64 returns the absolute value of `a` as a `f64` value.
// Example: assert fabs_64(-2.0) == f64(2.0)
[inline]
pub fn fabs_64(v f64) f64 {
	return if v > 0 { v } else { -v }
}
