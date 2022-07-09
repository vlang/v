// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module mt19937

import rand.buffer
import rand.seed

/*
C++ functions for MT19937, with initialization improved 2002/2/10.
   Coded by Takuji Nishimura and Makoto Matsumoto.
   This is a faster version by taking Shawn Cokus's optimization,
   Matthe Bellew's simplification, Isaku Wada's real version.

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote
        products derived from this software without specific prior written
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
*/
pub const seed_len = 2

const (
	nn            = 312
	mm            = 156
	matrix_a      = 0xB5026F5AA96619E9
	um            = 0xFFFFFFFF80000000
	lm            = 0x7FFFFFFF
	inv_f64_limit = 1.0 / 9007199254740992.0
)

// MT19937RNG is generator that uses the Mersenne Twister algorithm with period 2^19937.
// **NOTE**: The RNG is not seeded when instantiated so remember to seed it before use.
pub struct MT19937RNG {
	buffer.PRNGBuffer
mut:
	state []u64 = get_first_state(seed.time_seed_array(2))
	mti   int   = mt19937.nn
}

fn get_first_state(seed_data []u32) []u64 {
	mut state := []u64{len: mt19937.nn}
	calculate_state(seed_data, mut state)
	return state
}

// calculate_state returns a random state array calculated from the `seed_data`.
fn calculate_state(seed_data []u32, mut state []u64) []u64 {
	lo := u64(seed_data[0])
	hi := u64(seed_data[1])
	state[0] = u64((hi << 32) | lo)
	for j := 1; j < mt19937.nn; j++ {
		state[j] = u64(6364136223846793005) * (state[j - 1] ^ (state[j - 1] >> 62)) + u64(j)
	}
	return *state
}

// seed sets the current random state based on `seed_data`.
// seed expects `seed_data` to be only two `u32`s in little-endian format as [lower, higher].
pub fn (mut rng MT19937RNG) seed(seed_data []u32) {
	if seed_data.len != 2 {
		eprintln('mt19937 needs only two 32bit integers as seed: [lower, higher]')
		exit(1)
	}
	rng.state = calculate_state(seed_data, mut rng.state)
	rng.mti = mt19937.nn
	rng.bytes_left = 0
	rng.buffer = 0
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
[inline]
pub fn (mut rng MT19937RNG) u8() u8 {
	if rng.bytes_left >= 1 {
		rng.bytes_left -= 1
		value := u8(rng.buffer)
		rng.buffer >>= 8
		return value
	}
	rng.buffer = rng.u64()
	rng.bytes_left = 7
	value := u8(rng.buffer)
	rng.buffer >>= 8
	return value
}

// u16 returns a pseudorandom 16bit int in range `[0, 2¹⁶)`.
[inline]
pub fn (mut rng MT19937RNG) u16() u16 {
	if rng.bytes_left >= 2 {
		rng.bytes_left -= 2
		value := u16(rng.buffer)
		rng.buffer >>= 16
		return value
	}
	ans := rng.u64()
	rng.buffer = ans >> 16
	rng.bytes_left = 6
	return u16(ans)
}

// u32 returns a pseudorandom 32bit int in range `[0, 2³²)`.
[inline]
pub fn (mut rng MT19937RNG) u32() u32 {
	// Can we take a whole u32 out of the buffer?
	if rng.bytes_left >= 4 {
		rng.bytes_left -= 4
		value := u32(rng.buffer)
		rng.buffer >>= 32
		return value
	}
	ans := rng.u64()
	rng.buffer = ans >> 32
	rng.bytes_left = 4
	return u32(ans)
}

const mag01 = [u64(0), u64(matrix_a)]

// u64 returns a pseudorandom 64bit int in range `[0, 2⁶⁴)`.
[direct_array_access; inline]
pub fn (mut rng MT19937RNG) u64() u64 {
	mut x := u64(0)
	mut i := int(0)
	if rng.mti >= mt19937.nn {
		for i = 0; i < mt19937.nn - mt19937.mm; i++ {
			x = (rng.state[i] & mt19937.um) | (rng.state[i + 1] & mt19937.lm)
			rng.state[i] = rng.state[i + mt19937.mm] ^ (x >> 1) ^ mt19937.mag01[int(x & 1)]
		}
		for i < mt19937.nn - 1 {
			x = (rng.state[i] & mt19937.um) | (rng.state[i + 1] & mt19937.lm)
			rng.state[i] = rng.state[i + (mt19937.mm - mt19937.nn)] ^ (x >> 1) ^ mt19937.mag01[int(x & 1)]
			i++
		}
		x = (rng.state[mt19937.nn - 1] & mt19937.um) | (rng.state[0] & mt19937.lm)
		rng.state[mt19937.nn - 1] = rng.state[mt19937.mm - 1] ^ (x >> 1) ^ mt19937.mag01[int(x & 1)]
		rng.mti = 0
	}
	x = rng.state[rng.mti]
	rng.mti++
	x ^= (x >> 29) & 0x5555555555555555
	x ^= (x << 17) & 0x71D67FFFEDA60000
	x ^= (x << 37) & 0xFFF7EEE000000000
	x ^= (x >> 43)
	return x
}

// block_size returns the number of bits that the RNG can produce in a single iteration.
[inline]
pub fn (mut rng MT19937RNG) block_size() int {
	return 64
}

// free should be called when the generator is no longer needed
[unsafe]
pub fn (mut rng MT19937RNG) free() {
	unsafe { free(rng) }
}
