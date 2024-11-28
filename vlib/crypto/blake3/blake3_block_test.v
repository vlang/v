// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package blake3 implements the Blake3 cryptographic hash
// as described in:
// https://github.com/BLAKE3-team/BLAKE3-specs/blob/master/blake3.pdf
// Version 20211102173700

module blake3

fn test_mixing_function_g() {
	mut v := [u32(0xfc8acca9), 0xf912414a, 0x35d175e3, 0xe9ed298f, 0xbe57eb01, 0x60ea4e71, 0x66decd93,
		0xba6def8c, 0x0ba8bc5e, 0xec33f9fc, 0x6a2a29c9, 0x85c54e27, 0x3b719f82, 0x4a59df4a,
		0x0585477e, 0xf77a2e5a]

	v_result := [u32(0xfc8acca9), 0x0b9ea76f, 0x35d175e3, 0xe9ed298f, 0xbe57eb01, 0x60ea4e71,
		0x5a44ad65, 0xba6def8c, 0x0ba8bc5e, 0xec33f9fc, 0x6a2a29c9, 0xcd574ab5, 0x53f80752,
		0x4a59df4a, 0x0585477e, 0xf77a2e5a]

	a := u8(1)
	b := u8(6)
	c := u8(11)
	d := u8(12)

	x := u32(0x6e5c5d3e)
	y := u32(0x4e4f433c)

	g(mut v, a, b, c, d, x, y)

	for i, value in v {
		assert value == v_result[i], 'i: ${i}, left: ${value:08x} right: ${v_result[i]:08x}'
	}
}

fn test_mixing_round_function() {
	mut v := [u32(0xeb9ebdcd), 0x7b78363e, 0xcdb63957, 0x4da2219b, 0x4120ce20, 0x8e7f2c43, 0x08d57788,
		0x582d61ae, 0x96a4b4a3, 0xea904642, 0x92d806eb, 0x1fac731f, 0x74ccfd6d, 0x40f3ddcc,
		0x311ee8c0, 0x7936b8d3]

	m := [u32(0x20202020), 0x20202020, 0x22202020, 0x4e4f433c, 0x54584554, 0x6e5c5d3e, 0x0a3b2922,
		0x20202020, 0x65722020, 0x6e727574, 0x0a3b3020, 0x20202020, 0x6c65207d, 0x69206573,
		0x73282066, 0x6d637274]

	v_result := [u32(0x0c1813b2), 0x4a886b06, 0xdb196433, 0x2e4d5e82, 0x2d08943e, 0xf911603e,
		0x0e20a47d, 0xa00daed9, 0x9cb88560, 0xc4ae5e00, 0x44e3674e, 0xb8ef13fb, 0xecac5dd5,
		0xce1d693f, 0xb764dd49, 0xdff51e68]

	mixing_round(mut v, m, sigma[2])

	for i, value in v {
		assert value == v_result[i], 'i: ${i}, left: ${value:08x} right: ${v_result[i]:08x}'
	}
}

fn test_compress_function_f() {
	chaining_value := [u32(0x3d9e4dee), 0x6c2a8c01, 0xfd541471, 0x01672420, 0x8f8384b5, 0xba5f1566,
		0xf873b14b, 0xbb8bea12]

	block_words := [u32(0x72612870), 0x315b7667, 0x22202c5d, 0x656c2d2d, 0x2029226e, 0x30203d3d,
		0x0a7b2029, 0x20202020, 0x756f2020, 0x74757074, 0x6e656c5f, 0x28203d20, 0x657a6973,
		0x7329745f, 0x6f747274, 0x61286c6c]

	counter := u64(1)
	block_len := u32(64)
	flags := u32(0x00000000)

	expected_words := [u32(0x563aba7f), 0x5e699e49, 0xb9b7b6ee, 0x321df3da, 0x1f42bdd9, 0xd11fd7aa,
		0xf68c53a5, 0x510e6414, 0x3d5bd0ed, 0xe0f24ad4, 0x69cf12b4, 0xc2cd23cb, 0x5b8c9993,
		0x2081d39e, 0x4b651bf9, 0xec98067b]

	words := f(chaining_value, block_words, counter, block_len, flags)

	for i, word in words {
		assert word == expected_words[i], 'i: ${i}, left: ${word:08x} right: ${expected_words[i]:08x}'
	}
}
