// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Utility helpers used across the module
module ascon

import encoding.binary

// clear_bytes clears the bytes of x in n byte
@[inline]
fn clear_bytes(x u64, n int) u64 {
	mut c := x
	for i := 0; i < n; i++ {
		c &= ~set_byte(0xff, i)
	}
	return c
}

// pad appends a one followed by one or more zeroes to data
@[inline]
fn pad(n int) u64 {
	return u64(0x01) << (8 * n)
}

// assume input.len < 8
fn u64_from_partial_bytes(input []u8) u64 {
	mut tmp := []u8{len: 8}
	ct_copy_first(mut tmp, input)
	return binary.little_endian_u64(tmp)
}

// ct_copy_at copies of y into x start from at position in constant-time manner.
fn ct_copy_at(mut x []u8, y []u8, at int) {
	ct_copy_internal(1, mut x, y, at)
}

// ct_copy_first copies of y into first y.len of x in constant-time manner.
fn ct_copy_first(mut x []u8, y []u8) {
	ct_copy_internal(1, mut x, y, 0)
}

@[direct_array_access]
fn ct_copy_internal(v int, mut x []u8, y []u8, at int) {
	if at > x.len {
		panic('at > x.len')
	}
	if i64(x.len) < i64(y.len) + i64(at) {
		panic('invalid pos')
	}
	if x.len < y.len {
		panic('length x < y')
	}
	xmask := u8(v - 1)
	ymask := u8(~(v - 1))
	for i := 0; i < y.len; i++ {
		x[i + at] = x[i + at] & xmask | y[i] & ymask
	}
}

// portable little-endian helper
@[inline]
fn u64le(x u64) u64 {
	$if little_endian {
		return x
	}
	// otherwise, change into little-endian format
	return ((u64(0x00000000000000FF) & x) << 56) | ((u64(0x000000000000FF00) & x) << 40) | ((u64(0x0000000000FF0000) & x) << 24) | ((u64(0x00000000FF000000) & x) << 8) | ((u64(0x000000FF00000000) & x) >> 8) | ((u64(0x0000FF0000000000) & x) >> 24) | ((u64(0x00FF000000000000) & x) >> 40) | ((u64(0xFF00000000000000) & x) >> 56)
}

@[inline]
fn get_byte(x u64, i int) u8 {
	return u8(x >> (8 * i))
}

@[inline]
fn set_byte(b u8, i int) u64 {
	return u64(b) << (8 * i)
}

// load_bytes load partial bytes with length n, used internally.
@[direct_array_access]
fn load_bytes(bytes []u8, n int) u64 {
	mut x := u64(0)
	for i := 0; i < n; i++ {
		x |= set_byte(bytes[i], i)
	}
	return u64le(x)
}

fn store_bytes(mut out []u8, x u64, n int) {
	for i := 0; i < n; i++ {
		out[i] = get_byte(x, i)
	}
}
