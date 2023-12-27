// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package blake2b implements the Blake2b 512, 384, 256, and
// 160 bit hash algorithms
// as defined in IETF RFC 7693.
// Based off:   https://datatracker.ietf.org/doc/html/rfc7693
// Last updated: November 2015
module blake2b

import math.bits

// mixing function g
@[inline]
fn g(mut v []u64, a u8, b u8, c u8, d u8, x u64, y u64) {
	v[a] = v[a] + v[b] + x
	v[d] = bits.rotate_left_64((v[d] ^ v[a]), nr1)
	v[c] = v[c] + v[d]
	v[b] = bits.rotate_left_64((v[b] ^ v[c]), nr2)
	v[a] = v[a] + v[b] + y
	v[d] = bits.rotate_left_64((v[d] ^ v[a]), nr3)
	v[c] = v[c] + v[d]
	v[b] = bits.rotate_left_64((v[b] ^ v[c]), nr4)
}

// one complete mixing round with the function g
@[inline]
fn (d Digest) mixing_round(mut v []u64, s []u8) {
	g(mut v, 0, 4, 8, 12, d.m[s[0]], d.m[s[1]])
	g(mut v, 1, 5, 9, 13, d.m[s[2]], d.m[s[3]])
	g(mut v, 2, 6, 10, 14, d.m[s[4]], d.m[s[5]])
	g(mut v, 3, 7, 11, 15, d.m[s[6]], d.m[s[7]])

	g(mut v, 0, 5, 10, 15, d.m[s[8]], d.m[s[9]])
	g(mut v, 1, 6, 11, 12, d.m[s[10]], d.m[s[11]])
	g(mut v, 2, 7, 8, 13, d.m[s[12]], d.m[s[13]])
	g(mut v, 3, 4, 9, 14, d.m[s[14]], d.m[s[15]])
}

// compression function f
fn (mut d Digest) f(f bool) {
	// initialize the working vector
	mut v := []u64{len: 0, cap: 16}
	v << d.h[..8]
	v << iv[..8]

	v[12] ^= d.t.lo
	v[13] ^= d.t.hi

	if f {
		v[14] = ~v[14]
	}

	// go 12 rounds of cryptographic mixing
	d.mixing_round(mut v, sigma[0])
	d.mixing_round(mut v, sigma[1])
	d.mixing_round(mut v, sigma[2])
	d.mixing_round(mut v, sigma[3])
	d.mixing_round(mut v, sigma[4])
	d.mixing_round(mut v, sigma[5])
	d.mixing_round(mut v, sigma[6])
	d.mixing_round(mut v, sigma[7])
	d.mixing_round(mut v, sigma[8])
	d.mixing_round(mut v, sigma[9])
	d.mixing_round(mut v, sigma[0])
	d.mixing_round(mut v, sigma[1])

	// combine internal hash state with both halves of the working vector

	d.h[0] = d.h[0] ^ v[0] ^ v[8]
	d.h[1] = d.h[1] ^ v[1] ^ v[9]
	d.h[2] = d.h[2] ^ v[2] ^ v[10]
	d.h[3] = d.h[3] ^ v[3] ^ v[11]
	d.h[4] = d.h[4] ^ v[4] ^ v[12]
	d.h[5] = d.h[5] ^ v[5] ^ v[13]
	d.h[6] = d.h[6] ^ v[6] ^ v[14]
	d.h[7] = d.h[7] ^ v[7] ^ v[15]
}
