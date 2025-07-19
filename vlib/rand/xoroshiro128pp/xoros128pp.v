// Copyright (c) 2022 John Lloyd. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module xoroshiro128pp

import rand.seed
import rand.buffer

pub const seed_len = 4 // u32, that is

fn rotl(x u64, k int) u64 {
	return (x << k) | (x >> (64 - k))
}

// XOROS128PPRNG is ported from https://prng.di.unimi.it/xoroshiro128plusplus.c .
pub struct XOROS128PPRNG {
	buffer.PRNGBuffer
mut:
	state0 u64 = u64(0x853c49e6748fea9b) ^ seed.time_seed_64()
	state1 u64 = u64(0xda3e39cb94b95bdb) ^ seed.time_seed_64()
}

// seed seeds the XOROS128PPRNG with 4 `u32` values.
pub fn (mut rng XOROS128PPRNG) seed(seed_data []u32) {
	if seed_data.len != 4 {
		eprintln('XOROS128PPRNG needs 4 u32s to be seeded.')
		exit(1)
	}
	init_state0 := u64(seed_data[0]) | (u64(seed_data[1]) << 32)
	init_state1 := u64(seed_data[2]) | (u64(seed_data[3]) << 32)
	rng.state0 = init_state0
	rng.state1 = init_state1
	rng.u64()
	rng.u64()
	rng.bytes_left = 0
	rng.buffer = 0
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned `byte`.
@[inline]
pub fn (mut rng XOROS128PPRNG) u8() u8 {
	if rng.bytes_left >= 1 {
		rng.bytes_left -= 1
		value := u8(rng.buffer)
		rng.buffer >>= 8
		return value
	}
	ans := rng.u64()
	rng.buffer = ans >> 8
	rng.bytes_left = 7
	value := u8(ans)
	return value
}

// u16 returns a pseudorandom 16-bit unsigned integer (`u16`).
@[inline]
pub fn (mut rng XOROS128PPRNG) u16() u16 {
	if rng.bytes_left >= 2 {
		rng.bytes_left -= 2
		value := u16(rng.buffer)
		rng.buffer >>= 16
		return value
	}
	ans := rng.u64()
	rng.buffer = u32(ans >> 16)
	rng.bytes_left = 6
	return u16(ans)
}

// u32 returns a pseudorandom unsigned `u32`.
@[inline]
pub fn (mut rng XOROS128PPRNG) u32() u32 {
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

// u64 returns a pseudorandom 64-bit unsigned `u64`.
@[inline]
pub fn (mut rng XOROS128PPRNG) u64() u64 {
	oldstate0 := rng.state0
	mut oldstate1 := rng.state1
	res := rotl(oldstate0 + oldstate1, 17) + oldstate0
	oldstate1 = oldstate1 ^ oldstate0
	rng.state0 = rotl(oldstate0, 49) ^ oldstate1 ^ (oldstate1 << 21)
	rng.state1 = rotl(oldstate1, 28)
	return res
}

// block_size returns the number of bits that the RNG can produce in a single iteration.
@[inline]
pub fn (mut rng XOROS128PPRNG) block_size() int {
	return 64
}

// free should be called when the generator is no longer needed
@[unsafe]
pub fn (mut rng XOROS128PPRNG) free() {
	unsafe { free(rng) }
}
