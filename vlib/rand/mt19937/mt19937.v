// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module mt19937

import math.bits
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
const (
	nn            = 312
	mm            = 156
	matrix_a      = 0xB5026F5AA96619E9
	um            = 0xFFFFFFFF80000000
	lm            = 0x7FFFFFFF
	inv_f64_limit = 1.0 / 9007199254740992.0
)

// MT19937RNG is generator that uses the Mersenne Twister algorithm with period 2^19937.
pub struct MT19937RNG {
mut:
	state    []u64 = calculate_state(seed.time_seed_array(2), mut []u64{len: mt19937.nn})
	mti      int   = mt19937.nn
	next_rnd u32
	has_next bool
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
	rng.next_rnd = 0
	rng.has_next = false
}

// u32 returns a pseudorandom 32bit int in range `[0, 2³²)`.
[inline]
pub fn (mut rng MT19937RNG) u32() u32 {
	if rng.has_next {
		rng.has_next = false
		return rng.next_rnd
	}
	ans := rng.u64()
	rng.next_rnd = u32(ans >> 32)
	rng.has_next = true
	return u32(ans & 0xffffffff)
}

// u64 returns a pseudorandom 64bit int in range `[0, 2⁶⁴)`.
[inline]
pub fn (mut rng MT19937RNG) u64() u64 {
	mag01 := [u64(0), u64(mt19937.matrix_a)]
	mut x := u64(0)
	mut i := int(0)
	if rng.mti >= mt19937.nn {
		for i = 0; i < mt19937.nn - mt19937.mm; i++ {
			x = (rng.state[i] & mt19937.um) | (rng.state[i + 1] & mt19937.lm)
			rng.state[i] = rng.state[i + mt19937.mm] ^ (x >> 1) ^ mag01[int(x & 1)]
		}
		for i < mt19937.nn - 1 {
			x = (rng.state[i] & mt19937.um) | (rng.state[i + 1] & mt19937.lm)
			rng.state[i] = rng.state[i + (mt19937.mm - mt19937.nn)] ^ (x >> 1) ^ mag01[int(x & 1)]
			i++
		}
		x = (rng.state[mt19937.nn - 1] & mt19937.um) | (rng.state[0] & mt19937.lm)
		rng.state[mt19937.nn - 1] = rng.state[mt19937.mm - 1] ^ (x >> 1) ^ mag01[int(x & 1)]
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

// int returns a 32-bit signed (possibly negative) `int`.
[inline]
pub fn (mut rng MT19937RNG) int() int {
	return int(rng.u32())
}

// i64 returns a 64-bit signed (possibly negative) `i64`.
[inline]
pub fn (mut rng MT19937RNG) i64() i64 {
	return i64(rng.u64())
}

// int31 returns a 31bit positive pseudorandom `int`.
[inline]
pub fn (mut rng MT19937RNG) int31() int {
	return int(rng.u32() >> 1)
}

// int63 returns a 63bit positive pseudorandom `i64`.
[inline]
pub fn (mut rng MT19937RNG) int63() i64 {
	return i64(rng.u64() >> 1)
}

// u32n returns a 32bit `u32` in range `[0, max)`.
[inline]
pub fn (mut rng MT19937RNG) u32n(max u32) u32 {
	if max == 0 {
		eprintln('max must be positive integer.')
		exit(1)
	}
	// Check SysRNG in system_rng.c.v for explanation
	bit_len := bits.len_32(max)
	if bit_len == 32 {
		for {
			value := rng.u32()
			if value < max {
				return value
			}
		}
	} else {
		mask := (u32(1) << (bit_len + 1)) - 1
		for {
			value := rng.u32() & mask
			if value < max {
				return value
			}
		}
	}
	return u32(0)
}

// u64n returns a 64bit `u64` in range `[0, max)`.
[inline]
pub fn (mut rng MT19937RNG) u64n(max u64) u64 {
	if max == 0 {
		eprintln('max must be positive integer.')
		exit(1)
	}
	bit_len := bits.len_64(max)
	if bit_len == 64 {
		for {
			value := rng.u64()
			if value < max {
				return value
			}
		}
	} else {
		mask := (u64(1) << (bit_len + 1)) - 1
		for {
			value := rng.u64() & mask
			if value < max {
				return value
			}
		}
	}
	return u64(0)
}

// u32n returns a pseudorandom `u32` value that is guaranteed to be in range `[min, max)`.
[inline]
pub fn (mut rng MT19937RNG) u32_in_range(min u32, max u32) u32 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.u32n(max - min)
}

// u64n returns a pseudorandom `u64` value that is guaranteed to be in range `[min, max)`.
[inline]
pub fn (mut rng MT19937RNG) u64_in_range(min u64, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.u64n(max - min)
}

// intn returns a 32bit positive `int` in range `[0, max)`.
[inline]
pub fn (mut rng MT19937RNG) intn(max int) int {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return int(rng.u32n(u32(max)))
}

// i64n returns a 64bit positive `i64` in range `[0, max)`.
[inline]
pub fn (mut rng MT19937RNG) i64n(max i64) i64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return i64(rng.u64n(u64(max)))
}

// int_in_range returns a 32bit positive `int` in range `[min, max)`.
[inline]
pub fn (mut rng MT19937RNG) int_in_range(min int, max int) int {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.intn(max - min)
}

// i64_in_range returns a 64bit positive `i64` in range `[min, max)`.
[inline]
pub fn (mut rng MT19937RNG) i64_in_range(min i64, max i64) i64 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.i64n(max - min)
}

// f32 returns a 32bit real (`f32`) in range `[0, 1)`.
[inline]
pub fn (mut rng MT19937RNG) f32() f32 {
	return f32(rng.f64())
}

// f64 returns 64bit real (`f64`) in range `[0, 1)`.
[inline]
pub fn (mut rng MT19937RNG) f64() f64 {
	return f64(rng.u64() >> 11) * mt19937.inv_f64_limit
}

// f32n returns a 32bit real (`f32`) in range [0, max)`.
[inline]
pub fn (mut rng MT19937RNG) f32n(max f32) f32 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f32() * max
}

// f64n returns a 64bit real (`f64`) in range `[0, max)`.
[inline]
pub fn (mut rng MT19937RNG) f64n(max f64) f64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f64() * max
}

// f32_in_range returns a pseudorandom `f32` that lies in range `[min, max)`.
[inline]
pub fn (mut rng MT19937RNG) f32_in_range(min f32, max f32) f32 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.f32n(max - min)
}

// i64_in_range returns a pseudorandom `i64` that lies in range `[min, max)`.
[inline]
pub fn (mut rng MT19937RNG) f64_in_range(min f64, max f64) f64 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.f64n(max - min)
}
