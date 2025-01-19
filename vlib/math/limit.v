// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

// maxof returns the maximum value of the type `T`
@[inline]
pub fn maxof[T]() T {
	$if T is i8 {
		return max_i8
	} $else $if T is i16 {
		return max_i16
	} $else $if T is i32 {
		return max_i32
	} $else $if T is i32 {
		return max_i32
	} $else $if T is i64 {
		return max_i64
	} $else $if T is u8 {
		return max_u8
	} $else $if T is byte {
		return max_u8
	} $else $if T is u16 {
		return max_u16
	} $else $if T is u32 {
		return max_u32
	} $else $if T is u64 {
		return max_u64
	} $else $if T is f32 {
		return max_f32
	} $else $if T is f64 {
		return max_f64
	} $else $if T is int {
		$if new_int ? {
			return int(max_i64)
		}
		return int(max_i32)
	} $else {
		panic('A maximum value of the type `' + typeof[T]().name + '` is not defined.')
	}
}

// minof returns the minimum value of the type `T`
@[inline]
pub fn minof[T]() T {
	$if T is i8 {
		return min_i8
	} $else $if T is i16 {
		return min_i16
	} $else $if T is i32 {
		return min_i32
	} $else $if T is i32 {
		return min_i32
	} $else $if T is i64 {
		return min_i64
	} $else $if T is u8 {
		return min_u8
	} $else $if T is byte {
		return min_u8
	} $else $if T is u16 {
		return min_u16
	} $else $if T is u32 {
		return min_u32
	} $else $if T is u64 {
		return min_u64
	} $else $if T is f32 {
		return -max_f32
	} $else $if T is f64 {
		return -max_f64
	} $else $if T is int {
		$if new_int ? {
			return int(min_i64)
		}
		return int(min_i32)
	} $else {
		panic('A minimum value of the type `' + typeof[T]().name + '` is not defined.')
	}
}
