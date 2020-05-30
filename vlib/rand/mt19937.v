module rand

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
   http://www.math.keio.ac.jp/matumoto/emt.html
   email: matumoto@math.keio.ac.jp
*/
const (
	nn       = 312
	mm       = 156
	matrix_a = 0xB5026F5AA96619E9
	um       = 0xFFFFFFFF80000000
	lm       = 0x7FFFFFFF
)

pub struct MT19937Rng {
mut:
	state    [312]u64
	mti      int
	seed_set bool
	next_rnd u32
	pos      int
}

// seed() - Set the seed, needs only one int32
pub fn (mut rng MT19937Rng) seed(seed_data []u32) {
	if seed_data.len != 2 {
		eprintln("mt19937 needs only two 32bit integers as seed: [lower, higher]")
		exit(1)
	}
	lo := u64(seed_data[0])
	hi := u64(seed_data[1])
	rng.state[0] = u64((hi << 32) | lo)
	for j := 1; j < nn; j++ {
		rng.state[j] = u64(6364136223846793005) * (rng.state[j - 1] ^ (rng.state[j - 1] >>
			62)) + u64(j)
	}
	rng.mti = nn
	rng.seed_set = true
	rng.next_rnd = 0
	rng.pos = 0
}

// rng.u32() - return a pseudorandom 32bit int in [0, 2**32)
pub fn (mut rng MT19937Rng) u32() u32 {
	if rng.pos == 1 {
		rng.pos = 0
		return rng.next_rnd
	}
	ans := rng.u64()
	rng.next_rnd = u32(ans >> 32)
	return u32(ans & 0xffffffff)
}

// rng.u64() - return two u32's as little endian u64
pub fn (mut rng MT19937Rng) u64() u64 {
	mag01 := [u64(0), u64(matrix_a)]
	mut x := u64(0)
	mut i := int(0)
	if !rng.seed_set {
		rng.seed([u32(5489), u32(0)])
	}
	if rng.mti >= nn {
		for i = 0; i < nn - mm; i++ {
			x = (rng.state[i] & um) | (rng.state[i + 1] & lm)
			rng.state[i] = rng.state[i + mm] ^ (x >> 1) ^ mag01[int(x & 1)]
		}
		for i < nn - 1 {
			x = (rng.state[i] & um) | (rng.state[i + 1] & lm)
			rng.state[i] = rng.state[i + (mm - nn)] ^ (x >> 1) ^ mag01[int(x & 1)]
			i++
		}
		x = (rng.state[nn - 1] & um) | (rng.state[0] & lm)
		rng.state[nn - 1] = rng.state[mm - 1] ^ (x >> 1) ^ mag01[int(x & 1)]
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

// rng.int31() - return a 31bit positive pseudorandom integer
pub fn (mut rng MT19937Rng) int31() int {
	return int(rng.u32() >> 1)
}

// rng.int63() - return a 63bit positive pseudorandom integer
pub fn (mut rng MT19937Rng) int63() i64 {
	return i64(rng.u64() >> 1)
}

// rng.f64() - return 64bit real in [0, 1)
pub fn (mut rng MT19937Rng) f64() f64 {
	return f64(rng.u64() >> 11) * (1.0 / 9007199254740992.0)
}

// rng.f32() - return a 32bit real in [0, 1)
pub fn (mut rng MT19937Rng) f32() f32 {
	return f32(rng.f64())
}

// rng.u32n() - return a 32bit u32 in [0, max)
pub fn (mut rng MT19937Rng) u32n(max u32) u32 {
	if max <= 0 {
		eprintln('max must be positive integer')
		exit(1)
	}
	return rng.u32() % max
}

pub fn (mut rng MT19937Rng) u64n(max u64) u64 {
	if max <= 0 {
		eprintln('max must be positive integer')
		exit(1)
	}
	return rng.u64() % max
}

// rng.u32n(min, max) returns a pseudorandom u32 value that is guaranteed to be in [min, max)
[inline]
pub fn (mut rng MT19937Rng) u32_in_range(min, max u32) u32 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u32n(max - min)
}

// rng.u64n(min, max) returns a pseudorandom u64 value that is guaranteed to be in [min, max)
[inline]
pub fn (mut rng MT19937Rng) u64_in_range(min, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u64n(max - min)
}
