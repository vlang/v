// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module cipher

// NOTE: Implement other versions (joe-c)
// xor_bytes xors the bytes in a and b. The destination should have enough
// space, otherwise xor_bytes will panic. Returns the number of bytes xor'd.
pub fn xor_bytes(mut dst []u8, a []u8, b []u8) int {
	mut n := a.len
	if b.len < n {
		n = b.len
	}
	if n == 0 {
		return 0
	}
	safe_xor_bytes(mut dst, a, b, n)
	return n
}

// safe_xor_bytes XORs the bytes in `a` and `b` into `dst` it does so `n` times.
// Please note: `n` needs to be smaller or equal than the length of `a` and `b`.
pub fn safe_xor_bytes(mut dst []u8, a []u8, b []u8, n int) {
	for i in 0 .. n {
		dst[i] = a[i] ^ b[i]
	}
}

// xor_words XORs multiples of 4 or 8 bytes (depending on architecture.)
// The slice arguments `a` and `b` are assumed to be of equal length.
pub fn xor_words(mut dst []u8, a []u8, b []u8) {
	safe_xor_bytes(mut dst, a, b, b.len)
}
