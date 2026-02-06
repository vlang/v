// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package sha3 implements the 512, 384, 256, and 224
// bit hash algorithms and the 128 and 256 bit
// extended output functions as defined in
// https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf.
// Last updated: August 2015
module sha3

import encoding.binary
import math.bits

// when the state is 1600 bits, a lane is 64 bits
type Lane = u64

// the state is a 5 x 5 array of lanes
struct State {
mut:
	a [5][5]Lane
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
			unsafe {
				binary.little_endian_put_u64_at(mut byte_array, s.a[x][y], index)
			}
			index += 8
		}
	}

	return byte_array
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
			s.a[x][y] = binary.little_endian_u64_at(byte_array, index)
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
			s.a[x][y] ^= binary.little_endian_u64_at(byte_array, index)
			index += 8

			if index >= rate {
				return
			}
		}
	}
}

// kaccak_p_1600_24 performs 24 rounnds on a 1600 bit state.
//
@[direct_array_access]
fn (mut s State) kaccak_p_1600_24() {
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

		d0 := c4 ^ bits.rotate_left_64(c1, 1)
		d1 := c0 ^ bits.rotate_left_64(c2, 1)
		d2 := c1 ^ bits.rotate_left_64(c3, 1)
		d3 := c2 ^ bits.rotate_left_64(c4, 1)
		d4 := c3 ^ bits.rotate_left_64(c0, 1)

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
		b[0][1] = bits.rotate_left_64(s.a[3][0], 28)
		b[0][2] = bits.rotate_left_64(s.a[1][0], 1)
		b[0][3] = bits.rotate_left_64(s.a[4][0], 27)
		b[0][4] = bits.rotate_left_64(s.a[2][0], 62)

		b[1][0] = bits.rotate_left_64(s.a[1][1], 44)
		b[1][1] = bits.rotate_left_64(s.a[4][1], 20)
		b[1][2] = bits.rotate_left_64(s.a[2][1], 6)
		b[1][3] = bits.rotate_left_64(s.a[0][1], 36)
		b[1][4] = bits.rotate_left_64(s.a[3][1], 55)

		b[2][0] = bits.rotate_left_64(s.a[2][2], 43)
		b[2][1] = bits.rotate_left_64(s.a[0][2], 3)
		b[2][2] = bits.rotate_left_64(s.a[3][2], 25)
		b[2][3] = bits.rotate_left_64(s.a[1][2], 10)
		b[2][4] = bits.rotate_left_64(s.a[4][2], 39)

		b[3][0] = bits.rotate_left_64(s.a[3][3], 21)
		b[3][1] = bits.rotate_left_64(s.a[1][3], 45)
		b[3][2] = bits.rotate_left_64(s.a[4][3], 8)
		b[3][3] = bits.rotate_left_64(s.a[2][3], 15)
		b[3][4] = bits.rotate_left_64(s.a[0][3], 41)

		b[4][0] = bits.rotate_left_64(s.a[4][4], 14)
		b[4][1] = bits.rotate_left_64(s.a[2][4], 61)
		b[4][2] = bits.rotate_left_64(s.a[0][4], 18)
		b[4][3] = bits.rotate_left_64(s.a[3][4], 56)
		b[4][4] = bits.rotate_left_64(s.a[1][4], 2)

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
	0x8000000080008000, 0x000000000000808b, 0x0000000080000001, 0x8000000080008081,
	0x8000000000008009, 0x000000000000008a, 0x0000000000000088, 0x0000000080008009,
	0x000000008000000a, 0x000000008000808b, 0x800000000000008b, 0x8000000000008089,
	0x8000000000008003, 0x8000000000008002, 0x8000000000000080, 0x000000000000800a,
	0x800000008000000a, 0x8000000080008081, 0x8000000000008080, 0x0000000080000001,
	0x8000000080008008]

fn (s State) str() string {
	mut output := '\n             y = 0            y = 1            y = 2            y = 3            y = 4\n'
	for x in 0 .. 5 {
		output += 'x = ${x}: ${s.a[x][0]:016x} ${s.a[x][1]:016x} ${s.a[x][2]:016x} ${s.a[x][3]:016x} ${s.a[x][4]:016x}\n'
	}

	return output
}
