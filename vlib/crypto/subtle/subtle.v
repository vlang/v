// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package subtle implements functions that are often useful in cryptographic
// code but require careful thought to use correctly.
module subtle

// constant_time_compare returns 1 if the two slices, x and y, have equal contents
// and 0 otherwise. The time taken is a fn tion of the length of the slices and
// is independent of the contents.
fn  constant_time_compare(x, y []byte) int {
	if x.len != y.len {
		return 0
	}

	mut v := byte(0)

	for i := 0; i < x.len; i++ {
		v |= x[i] ^ y[i]
	}

	return constant_time_byte_eq(v, 0)
}

// constant_time_select returns x if v == 1 and y if v == 0.
// Its behavior is undefined if v takes any other value.
fn  constant_time_select(v, x, y int) int {
	return ~(v-1)&x | (v-1)&y
}

// constant_time_byte_eq returns 1 if x == y and 0 otherwise.
fn  constant_time_byte_eq(x, y byte) int {
	return int((u32(x^y) - u32(1)) >> u32(31))
}

// constant_time_eq returns 1 if x == y and 0 otherwise.
fn  constant_time_eq(x, y i32) int {
	return int((u64(u32(x^y)) - u64(1)) >> u64(63))
}

// constant_time_copy copies the contents of y into x (a slice of equal length)
// if v == 1. If v == 0, x is left unchanged. Its behavior is undefined if v
// takes any other value.
fn  constant_time_copy(v int, x, y []byte) {
	if x.len != y.len {
		panic('subtle: slices have different lengths')
	}

	xmask := byte(v - 1)
	ymask := byte(~(v - 1))
	for i := 0; i < x.len; i++ {
		x[i] = x[i]&xmask | y[i]&ymask
	}
}

// constant_time_less_or_eq returns 1 if x <= y and 0 otherwise.
// Its behavior is undefined if x or y are negative or > 2**31 - 1.
fn  constant_time_less_or_eq(x, y int) int {
	x32 := i32(x)
	y32 := i32(y)
	return int(((x32 - y32 - i32(1)) >> i32(31)) & 1)
}