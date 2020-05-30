module rand

import time

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

pub struct MT19937Rng {
mut:
	state [624]u32
	left int
	initf int
	next int
}

// seed() - Set the seed, needs only one int32
pub fn (mut rng MT19937Rng) seed(seed_data []u32) {
	if seed_data.len != 1{
		eprintln("mt19937 needs only one 32bit seed")
		exit(1)
	}
	rng.state[0] = seed_data[0]
	for j := 1; j < 624; j++ {
		rng.state[j] = u32(1812433253 * (rng.state[j-1] ^ (rng.state[j-1] >> 30)) + u32(j))
	}
	rng.left = 1
	rng.initf = 1
}

[inline]
fn mix_bits(a, b u32) u32 {
	return (a & 0x80000000) | (b & 0x7fffffff)
}

[inline]
fn twist(a, b u32) u32 {
	return (mix_bits(a, b)>>1) ^ (0x9908b0df * (b & 1))
}

fn (mut rng MT19937Rng) next_state() {
	mut pos := 0
	if rng.initf == 0 {
		rng.seed([u32(C.time(0))])
	}
	rng.left = 624
	rng.next = 0
	for j := 624-397; j > 0; j-- {
		rng.state[pos] = rng.state[pos+397]^twist(rng.state[pos], rng.state[pos+1])
		pos++
	}
	for j := 397-1; j > 0; j-- {
		rng.state[pos] = rng.state[397-624+pos]^twist(rng.state[pos], rng.state[pos+1])
		pos++
	}
	rng.state[pos] = rng.state[397-624+pos]^twist(rng.state[pos], rng.state[0])
}

// rng.u32() - return a pseudorandom 32bit int in [0, 2**32)
pub fn (mut rng MT19937Rng) u32() u32 {
	rng.left--
	if rng.left == 0 {
		rng.next_state()
	}
	mut y := rng.state[rng.next]
	rng.next++
	y ^= (y >> 11)
    y ^= (y << 7) & 0x9d2c5680
    y ^= (y << 15) & 0xefc60000
    y ^= (y >> 18)
	return y
}

pub fn (mut rng MT19937Rng) u64() u64 {
	// fallback to u32
	return u64(rng.u32())
}

// rng.int31() - return a 31bit positive pseudorandom integer
pub fn (mut rng MT19937Rng) int31() int {
	return int(rng.u32() >> 1)
}

// rng.f64() - return 64bit real in [0, 1)
pub fn (mut rng MT19937Rng) f64() f64 {
	return f64(rng.u32())*(1.0/4294967296.0)
}

// rng.f32() - return a 32bit real in [0, 1)
pub fn (mut rng MT19937Rng) f32() f32 {
	return f32(rng.u32())*f32(1.0/4294967296.0)
}

// rng.u32n() - return a 32bit u32 in [0, max)
pub fn (mut rng MT19937Rng) u32n(max u32) u32 {
	if max < 0 {
		eprintln("max must be non-negative")
		exit(1)
	}
	return rng.u32()*max
}

pub fn (mut rng MT19937Rng) u64n(max u64) u64 {
	if max < 0 {
		eprintln("max must be non-negative")
		exit(1)
	}
	return rng.u64()*max
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