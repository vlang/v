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
	mut byte_array := []u8{len: 200, cap: 200}
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
// The loop is unrolled to get a little better performance.
fn (mut s State) kaccak_p_1600_24() {
	s.rnd(0)
	s.rnd(1)
	s.rnd(2)
	s.rnd(3)
	s.rnd(4)
	s.rnd(5)
	s.rnd(6)
	s.rnd(7)
	s.rnd(8)
	s.rnd(9)
	s.rnd(10)
	s.rnd(11)
	s.rnd(12)
	s.rnd(13)
	s.rnd(14)
	s.rnd(15)
	s.rnd(16)
	s.rnd(17)
	s.rnd(18)
	s.rnd(19)
	s.rnd(20)
	s.rnd(21)
	s.rnd(22)
	s.rnd(23)
}

// rnd is a single round of stepping functions.
//
// The definition of a round is the application of the stepping
// functions theta, rho, pi, chi, and iota, in order, on the
// state.  The round index also influences the outcome and is
// constrained to be 0 <= round_index < 24.
@[inline]
fn (mut s State) rnd(round_index int) {
	s.theta()
	s.rho()
	s.pi()
	s.chi()
	s.iota(round_index)
}

// theta is the first step mapping function.  It is defined as:
//
// 1. For all pairs (x, z) such that 0 <= x < 5 and 0 <= z < w, let
//    C[x, z] = A[x, 0, z] xor A[x, 1, z] xor A[x, 2, z] xor A[x, 3, z] xor A[x, 4, z].
// 2. For all pairs (x, z) such that 0 <= x < 5 and 0 <= z < w let
//    D[x, z] = C[(x-1) mod 5, z] xor C[(x+1) mod 5, (z – 1) mod w].
// 3. For all triples (x, y, z) such that 0 <= x < 5, 0 <= y < 5, and 0 <=≤ z < w, let
//    A′[x, y, z] = A[x, y, z] xor D[x, z].
//
// A is the 5 x 5 x w state matrix.  w is the number of bits in the z axis, 64 in our case.
//
// We can represent a lane from the state matrix as a u64 value and operate
// on all the bite in the lane with a single 64-bit operation.  And, since
// we represent a lane as a u64 value, we can reduce the state to a 2
// dimensional array of u64 values.
@[direct_array_access; inline]
fn (mut s State) theta() {
	// calculate the 5 intermediate C values
	mut c := [5]Lane{init: 0}
	for x in 0 .. 5 {
		for y in 0 .. 5 {
			c[x] ^= s.a[x][y]
		}
	}

	// calculate the 5 intermediate D values
	mut d := [5]Lane{init: 0}
	for x in 0 .. 5 {
		d[x] = bits.rotate_left_64(c[(x + 1) % 5], 1) ^ c[(x + 4) % 5]
	}

	// add the D values back into the state
	for x in 0 .. 5 {
		for y in 0 .. 5 {
			s.a[x][y] ^= d[x]
		}
	}
}

// rho_offsets are the amount of rotation to apply to a particular lane
// given its position in the state matrix.
const rho_offsets = [[int(0), 36, 3, 41, 18], [int(1), 44, 10, 45, 2],
	[int(62), 6, 43, 15, 61], [int(28), 55, 25, 21, 56], [int(27), 20, 39, 8, 14]]

// rho is the second step mapping function.  It is defined as:
//
// 1. For all z such that 0 <= z < w, let A′ [0, 0, z] = A[0, 0, z].
// 2. Let (x, y) = (1, 0).
// 3. For t from 0 to 23:
//      a. for all z such that 0 <= z < w, let A′[x, y, z] = A[x, y, (z – (t + 1)(t + 2)/2) mod w];
//      b. let (x, y) = (y, (2x + 3y) mod 5).
//
// A is the 5 x 5 x w state matrix.  w is the number of bits in the z axis, 64 in our case.
//
// Step 1 looks worthless since A' will be overwtitten by step 3a.
//
// Steps 2 and 3b are defining how the x and y values are initialized and updated
// as t goes from 0 to 23.  Notice that the initial value of x, y of 0, 0 is not
// calculated and is just zero.  The other 24 values needed are calculated,
// making a total of 25, which is the total number of lanes in the state.  By
// setting the offset at 0, 0 to 0, that lane does not get rotated.
//
// The effect of step 3a is to rotate a 64-bit lane by the amount calculated by
// (((t + 1) * (t + 2)) / 2) % 64.  In order to save time, these rotation values,
// called offsets, can be calculated ahead of time.
@[direct_array_access; inline]
fn (mut s State) rho() {
	for x in 0 .. 5 {
		for y in 0 .. 5 {
			s.a[x][y] = bits.rotate_left_64(s.a[x][y], rho_offsets[x][y])
		}
	}
}

// pi is the third step mapping function.  It is defined as:
//
// 1. For all triples (x, y, z) such that 0 <= x < 5, 0 <= y < 5, and 0 <= z < w, let
//    A′[x, y, z]= A[(x + 3y) mod 5, x, z].
//
// A is the 5 x 5 x w state matrix.  w is the number of bits in the z axis, 64 in our case.
//
// For this function, we will need to have a temporary version of the state for
// holding the rearranged lanes.
@[direct_array_access; inline]
fn (mut s State) pi() {
	mut a_prime := [5][5]Lane{}
	for x in 0 .. 5 {
		for y in 0 .. 5 {
			a_prime[x][y] = s.a[(x + (3 * y)) % 5][x]
		}
	}

	// make the temporary state be the returned state
	for x in 0 .. 5 {
		for y in 0 .. 5 {
			s.a[x][y] = a_prime[x][y]
		}
	}
}

// chi is the fourth step mapping function.  It is defined as:
//
// 1. For all triples (x, y, z) such that 0 <= x < 5, 0 <= y < 5, and 0 <= z < w, let
//    A′ [x, y, z] = A[x, y, z] xor ((A[(x+1) mod 5, y, z] xor 1) & A[(x+2) mod 5, y, z]).
//
// A is the 5 x 5 x w state matrix.  w is the number of bits in the z axis, 64 in our case.
//
// The effect of chi is to XOR each bit with a non-linear function of two other bits
// in its row.
//
// For this function, we will need to have a temporary version of the state for
// holding the changed lanes.
@[direct_array_access; inline]
fn (mut s State) chi() {
	mut a_prime := [5][5]Lane{}
	for x in 0 .. 5 {
		for y in 0 .. 5 {
			a_prime[x][y] = s.a[x][y] ^ (~(s.a[(x + 1) % 5][y]) & s.a[(x + 2) % 5][y])
		}
	}

	// make the temporary state be the returned state
	for x in 0 .. 5 {
		for y in 0 .. 5 {
			s.a[x][y] = a_prime[x][y]
		}
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

// iota is the fifth step mapping function.  It is defined as:
//
// 1. For all triples (x, y, z) such that 0 <= x < 5, 0 <= y < 5, and 0 <= z < w, let
//    A′[x, y, z] = A[x, y, z].
// 2. Let RC = 0**w
// 3. For j from 0 to l, let RC[2**j – 1] = rc(j + 7i_r).
// 4. For all z such that 0 ≤ z < w, let A′ [0, 0, z] = A′ [0, 0, z] xor RC[z].
//
// A is the 5 x 5 x w state matrix.  w is the number of bits in the z axis, 64 in our case.
//
// This is pretty ugly.  Fortunately, all the uglyness can be precomputed so that
// all we need to do is xor lane 0, 0 with the appropriate precomputed value.  These
// precomputed values are indexed by the round which is being applied.  For sha3,
// the number of rounds is 24 so we just need to precompute the 24 valuse needed
// to xor with lane 0, 0.
@[direct_array_access; inline]
fn (mut s State) iota(round_index int) {
	s.a[0][0] ^= iota_round_constants[round_index]
}

fn (s State) str() string {
	mut output := '\n             y = 0            y = 1            y = 2            y = 3            y = 4\n'
	for x in 0 .. 5 {
		output += 'x = ${x}: ${s.a[x][0]:016x} ${s.a[x][1]:016x} ${s.a[x][2]:016x} ${s.a[x][3]:016x} ${s.a[x][4]:016x}\n'
	}

	return output
}
