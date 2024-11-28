// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package blake3 implements the Blake3 cryptographic hash
// as described in:
// https://github.com/BLAKE3-team/BLAKE3-specs/blob/master/blake3.pdf
// Version 20211102173700

module blake3

import math.bits

// mixing function g
@[inline]
fn g(mut v []u32, a u8, b u8, c u8, d u8, x u32, y u32) {
	v[a] = v[a] + v[b] + x
	v[d] = bits.rotate_left_32((v[d] ^ v[a]), nr1)
	v[c] = v[c] + v[d]
	v[b] = bits.rotate_left_32((v[b] ^ v[c]), nr2)
	v[a] = v[a] + v[b] + y
	v[d] = bits.rotate_left_32((v[d] ^ v[a]), nr3)
	v[c] = v[c] + v[d]
	v[b] = bits.rotate_left_32((v[b] ^ v[c]), nr4)
}

// one complete mixing round with the function g
@[inline]
fn mixing_round(mut v []u32, m []u32, s []u8) {
	g(mut v, 0, 4, 8, 12, m[s[0]], m[s[1]])
	g(mut v, 1, 5, 9, 13, m[s[2]], m[s[3]])
	g(mut v, 2, 6, 10, 14, m[s[4]], m[s[5]])
	g(mut v, 3, 7, 11, 15, m[s[6]], m[s[7]])

	g(mut v, 0, 5, 10, 15, m[s[8]], m[s[9]])
	g(mut v, 1, 6, 11, 12, m[s[10]], m[s[11]])
	g(mut v, 2, 7, 8, 13, m[s[12]], m[s[13]])
	g(mut v, 3, 4, 9, 14, m[s[14]], m[s[15]])
}

// compression function f
fn f(h []u32, m []u32, counter u64, input_bytes u32, flags u32) []u32 {
	mut v := []u32{len: 0, cap: 16}

	// initialize the working vector
	v << h[..8]
	v << iv[..4]

	v << u32(counter & 0x00000000ffffffff)
	v << u32(counter >> 32)

	v << input_bytes

	v << flags

	// go 7 rounds of cryptographic mixing
	//
	// These could potentially be spawned in concurrent tasks
	// to see if there is any real speed improvement.
	mixing_round(mut v, m, sigma[0])
	mixing_round(mut v, m, sigma[1])
	mixing_round(mut v, m, sigma[2])
	mixing_round(mut v, m, sigma[3])
	mixing_round(mut v, m, sigma[4])
	mixing_round(mut v, m, sigma[5])
	mixing_round(mut v, m, sigma[6])

	// combine internal hash state with both halves of the working vector

	for i in 0 .. 8 {
		v[i] ^= v[i + 8]
		v[i + 8] ^= h[i]
	}

	return v
}
