// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package sha3 implements the 512, 384, 256, and 224
// bit hash algorithms and the 128 and 256 bit
// extended output functions as defined in
// https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf.
// Last updated: August 2015
module sha3

// when the state is 1600 bits, a lane is 64 bits
type Lane = u64

// the state is a 5 x 5 array of lanes
struct State {
mut:
	a [5][5]Lane
}

@[direct_array_access; inline]
fn read_u64_le_at(b []u8, offset int) u64 {
	_ = b[offset]
	_ = b[offset + 7]
	return u64(b[offset]) | (u64(b[offset + 1]) << 8) | (u64(b[offset + 2]) << 16) | (u64(b[offset + 3]) << 24) | (u64(b[offset + 4]) << 32) | (u64(b[offset + 5]) << 40) | (u64(b[offset + 6]) << 48) | (u64(b[offset + 7]) << 56)
}

@[direct_array_access; inline]
fn put_u64_le_at(mut b []u8, value u64, offset int) {
	_ = b[offset]
	_ = b[offset + 7]
	b[offset] = u8(value)
	b[offset + 1] = u8(value >> 8)
	b[offset + 2] = u8(value >> 16)
	b[offset + 3] = u8(value >> 24)
	b[offset + 4] = u8(value >> 32)
	b[offset + 5] = u8(value >> 40)
	b[offset + 6] = u8(value >> 48)
	b[offset + 7] = u8(value >> 56)
}

// to_bytes converts the state into a byte array
//
// A 1600 bit state fits into 200 bytes.
//
// The state consists of 25 u64 values arranged in a
// 5 x 5 array.
//
// The state is not modified by this method.
@[direct_array_access]
fn (s State) to_bytes() []u8 {
	mut byte_array := []u8{len: 200}
	mut index := 0

	for y in 0 .. 5 {
		for x in 0 .. 5 {
			put_u64_le_at(mut byte_array, s.a[x][y], index)
			index += 8
		}
	}

	return byte_array
}

// store_bytes writes the first `n` bytes of the state (n <= 200) into `b`, starting at `offset`.
@[direct_array_access]
fn (s &State) store_bytes(mut b []u8, offset int, n int) {
	full := n / 8
	for i in 0 .. full {
		put_u64_le_at(mut b, s.a[i % 5][i / 5], offset + i * 8)
	}
	if n % 8 != 0 {
		lane := s.a[full % 5][full / 5]
		for j in 0 .. n % 8 {
			b[offset + full * 8 + j] = u8(lane >> (8 * j))
		}
	}
}

// xor_block xors the first `rate` bytes of `b` (rate % 8 == 0) into the state.
@[direct_array_access]
fn (mut s State) xor_block(mut b [200]u8, rate int) {
	for i in 0 .. rate / 8 {
		o := i * 8
		s.a[i % 5][i / 5] ^= u64(b[o]) | (u64(b[o + 1]) << 8) | (u64(b[o + 2]) << 16) | (u64(b[o + 3]) << 24) | (u64(b[o + 4]) << 32) | (u64(b[o + 5]) << 40) | (u64(b[o + 6]) << 48) | (u64(b[o + 7]) << 56)
	}
}

// store_block writes the first `rate` bytes of the state (rate % 8 == 0) into `b`.
@[direct_array_access]
fn (s &State) store_block(mut b [200]u8, rate int) {
	for i in 0 .. rate / 8 {
		lane := s.a[i % 5][i / 5]
		for j in 0 .. 8 {
			b[i * 8 + j] = u8(lane >> (8 * j))
		}
	}
}

// from_bytes sets the state from an array of bytes
//
// A 1600 bit state fits into 200 bytes.  It is expected
// that the input byte array is 200 bytes long.
//
// All 25 u64 values are set from the input byte array.
@[direct_array_access]
fn (mut s State) from_bytes(byte_array []u8) {
	mut index := 0

	for y in 0 .. 5 {
		for x in 0 .. 5 {
			s.a[x][y] = read_u64_le_at(byte_array, index)
			index += 8
		}
	}
}

// xor_bytes xor's an array of bytes into the state
//
// This is how new data gets absorbed into the sponge.
//
// It is expected that the length of the byte array is
// the same as the rate.  The rate is how many bytes
// can be absorbed at one permutation of the state.
//
// The rate must be less than the size of the state
// in bytes.
@[direct_array_access]
fn (mut s State) xor_bytes(byte_array []u8, rate int) {
	mut index := 0

	for y in 0 .. 5 {
		for x in 0 .. 5 {
			s.a[x][y] ^= read_u64_le_at(byte_array, index)
			index += 8

			if index >= rate {
				return
			}
		}
	}
}

// keccak_p_1600_24 performs 24 rounds on a 1600 bit state.
//
// The rotations are written out instead of calling bits.rotate_left_64: when math.bits comes from
// the module cache, that call is not inlined, and the 29 calls per round dominate the permutation.
@[direct_array_access]
fn (mut s State) keccak_p_1600_24() {
	mut b := [5][5]Lane{}
	for round_index in 0 .. 24 {
		// theta
		// C[x] = A[x,0] xor A[x,1] xor A[x,2] xor A[x,3] xor A[x,4],   for x in 0…4
		// D[x] = C[x-1] xor rot(C[x+1],1),                             for x in 0…4
		// A[x,y] = A[x,y] xor D[x],                           for (x,y) in (0…4,0…4)
		c0 := s.a[0][0] ^ s.a[0][1] ^ s.a[0][2] ^ s.a[0][3] ^ s.a[0][4]
		c1 := s.a[1][0] ^ s.a[1][1] ^ s.a[1][2] ^ s.a[1][3] ^ s.a[1][4]
		c2 := s.a[2][0] ^ s.a[2][1] ^ s.a[2][2] ^ s.a[2][3] ^ s.a[2][4]
		c3 := s.a[3][0] ^ s.a[3][1] ^ s.a[3][2] ^ s.a[3][3] ^ s.a[3][4]
		c4 := s.a[4][0] ^ s.a[4][1] ^ s.a[4][2] ^ s.a[4][3] ^ s.a[4][4]

		d0 := c4 ^ ((c1 << 1) | (c1 >> 63))
		d1 := c0 ^ ((c2 << 1) | (c2 >> 63))
		d2 := c1 ^ ((c3 << 1) | (c3 >> 63))
		d3 := c2 ^ ((c4 << 1) | (c4 >> 63))
		d4 := c3 ^ ((c0 << 1) | (c0 >> 63))

		// vfmt off
		s.a[0][0] ^= d0 s.a[0][1] ^= d0 s.a[0][2] ^= d0 s.a[0][3] ^= d0 s.a[0][4] ^= d0
		s.a[1][0] ^= d1 s.a[1][1] ^= d1 s.a[1][2] ^= d1 s.a[1][3] ^= d1 s.a[1][4] ^= d1
		s.a[2][0] ^= d2 s.a[2][1] ^= d2 s.a[2][2] ^= d2	s.a[2][3] ^= d2	s.a[2][4] ^= d2
		s.a[3][0] ^= d3	s.a[3][1] ^= d3	s.a[3][2] ^= d3	s.a[3][3] ^= d3	s.a[3][4] ^= d3
		s.a[4][0] ^= d4	s.a[4][1] ^= d4	s.a[4][2] ^= d4	s.a[4][3] ^= d4	s.a[4][4] ^= d4
		// vfmt on

		// rho and pi
		// B[y,2*x+3*y] = rot(A[x,y], r[x,y]),                 for (x,y) in (0…4,0…4)
		b[0][0] = s.a[0][0]
		b[0][1] = ((s.a[3][0] << 28) | (s.a[3][0] >> 36))
		b[0][2] = ((s.a[1][0] << 1) | (s.a[1][0] >> 63))
		b[0][3] = ((s.a[4][0] << 27) | (s.a[4][0] >> 37))
		b[0][4] = ((s.a[2][0] << 62) | (s.a[2][0] >> 2))

		b[1][0] = ((s.a[1][1] << 44) | (s.a[1][1] >> 20))
		b[1][1] = ((s.a[4][1] << 20) | (s.a[4][1] >> 44))
		b[1][2] = ((s.a[2][1] << 6) | (s.a[2][1] >> 58))
		b[1][3] = ((s.a[0][1] << 36) | (s.a[0][1] >> 28))
		b[1][4] = ((s.a[3][1] << 55) | (s.a[3][1] >> 9))

		b[2][0] = ((s.a[2][2] << 43) | (s.a[2][2] >> 21))
		b[2][1] = ((s.a[0][2] << 3) | (s.a[0][2] >> 61))
		b[2][2] = ((s.a[3][2] << 25) | (s.a[3][2] >> 39))
		b[2][3] = ((s.a[1][2] << 10) | (s.a[1][2] >> 54))
		b[2][4] = ((s.a[4][2] << 39) | (s.a[4][2] >> 25))

		b[3][0] = ((s.a[3][3] << 21) | (s.a[3][3] >> 43))
		b[3][1] = ((s.a[1][3] << 45) | (s.a[1][3] >> 19))
		b[3][2] = ((s.a[4][3] << 8) | (s.a[4][3] >> 56))
		b[3][3] = ((s.a[2][3] << 15) | (s.a[2][3] >> 49))
		b[3][4] = ((s.a[0][3] << 41) | (s.a[0][3] >> 23))

		b[4][0] = ((s.a[4][4] << 14) | (s.a[4][4] >> 50))
		b[4][1] = ((s.a[2][4] << 61) | (s.a[2][4] >> 3))
		b[4][2] = ((s.a[0][4] << 18) | (s.a[0][4] >> 46))
		b[4][3] = ((s.a[3][4] << 56) | (s.a[3][4] >> 8))
		b[4][4] = ((s.a[1][4] << 2) | (s.a[1][4] >> 62))

		// chi
		// A[x,y] = B[x,y] xor ((not B[x+1,y]) and B[x+2,y]),  for (x,y) in (0…4,0…4)
		for y in 0 .. 5 {
			s.a[0][y] = b[0][y] ^ (~b[1][y] & b[2][y])
			s.a[1][y] = b[1][y] ^ (~b[2][y] & b[3][y])
			s.a[2][y] = b[2][y] ^ (~b[3][y] & b[4][y])
			s.a[3][y] = b[3][y] ^ (~b[4][y] & b[0][y])
			s.a[4][y] = b[4][y] ^ (~b[0][y] & b[1][y])
		}

		// iota
		// A[0,0] = A[0,0] xor RC
		s.a[0][0] ^= iota_round_constants[round_index]
	}
}

// iota_round_constants are precomputed xor masks for the iota function
const iota_round_constants = [u64(0x0000000000000001), 0x0000000000008082, 0x800000000000808a,
	0x8000000080008000, 0x000000000000808b, 0x0000000080000001, 0x8000000080008081, 0x8000000000008009,
	0x000000000000008a, 0x0000000000000088, 0x0000000080008009, 0x000000008000000a, 0x000000008000808b,
	0x800000000000008b, 0x8000000000008089, 0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
	0x000000000000800a, 0x800000008000000a, 0x8000000080008081, 0x8000000000008080, 0x0000000080000001,
	0x8000000080008008]
