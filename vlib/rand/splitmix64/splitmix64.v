// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module splitmix64

import rand.seed

pub const seed_len = 2

// SplitMix64RNG ported from http://xoshiro.di.unimi.it/splitmix64.c
pub struct SplitMix64RNG {
mut:
	state      u64 = seed.time_seed_64()
	bytes_left int
	buffer     u64
}

// seed sets the seed of the accepting SplitMix64RNG to the given data
// in little-endian format (i.e. lower 32 bits are in [0] and higher 32 bits in [1]).
pub fn (mut rng SplitMix64RNG) seed(seed_data []u32) {
	if seed_data.len != 2 {
		eprintln('SplitMix64RNG needs 2 32-bit unsigned integers as the seed.')
		exit(1)
	}
	rng.state = seed_data[0] | (u64(seed_data[1]) << 32)
	rng.bytes_left = 0
	rng.buffer = 0
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
[inline]
pub fn (mut rng SplitMix64RNG) byte() byte {
	if rng.bytes_left >= 1 {
		rng.bytes_left -= 1
		value := byte(rng.buffer)
		rng.buffer >>= 8
		return value
	}
	rng.buffer = rng.u64()
	rng.bytes_left = 7
	value := byte(rng.buffer)
	rng.buffer >>= 8
	return value
}

// u16 returns a pseudorandom 16bit int in range `[0, 2¹⁶)`.
[inline]
pub fn (mut rng SplitMix64RNG) u16() u16 {
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
pub fn (mut rng SplitMix64RNG) u32() u32 {
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

// u64 returns a pseudorandom 64bit int in range `[0, 2⁶⁴)`.
[inline]
pub fn (mut rng SplitMix64RNG) u64() u64 {
	rng.state += (0x9e3779b97f4a7c15)
	mut z := rng.state
	z = (z ^ (z >> u64(30))) * 0xbf58476d1ce4e5b9
	z = (z ^ (z >> u64(27))) * 0x94d049bb133111eb
	return z ^ (z >> (31))
}

// block_size returns the number of bits that the RNG can produce in a single iteration.
[inline]
pub fn (mut rng SplitMix64RNG) block_size() int {
	return 64
}

// free should be called when the generator is no longer needed
[unsafe]
pub fn (mut rng SplitMix64RNG) free() {
	unsafe { free(rng) }
}
