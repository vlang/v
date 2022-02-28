// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wyrand

import hash
import rand.seed

// Redefinition of some constants that we will need for pseudorandom number generation.
const (
	wyp0 = u64(0xa0761d6478bd642f)
	wyp1 = u64(0xe7037ed1a0b428db)
)

// WyRandRNG is a RNG based on the WyHash hashing algorithm.
pub struct WyRandRNG {
mut:
	state      u64 = seed.time_seed_64()
	bytes_left int
	buffer     u64
}

// seed sets the seed, needs only two `u32`s in little-endian format as [lower, higher].
pub fn (mut rng WyRandRNG) seed(seed_data []u32) {
	if seed_data.len != 2 {
		eprintln('WyRandRNG needs 2 32-bit unsigned integers as the seed.')
		exit(1)
	}
	rng.state = seed_data[0] | (u64(seed_data[1]) << 32)
	rng.bytes_left = 0
	rng.buffer = 0
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
[inline]
pub fn (mut rng WyRandRNG) byte() byte {
	// Can we extract a value from the buffer?
	if rng.bytes_left >= 1 {
		rng.bytes_left -= 1
		value := byte(rng.buffer)
		rng.buffer >>= 8
		return value
	}
	// Add a new value to the buffer
	rng.buffer = rng.u64()
	rng.bytes_left = 7
	value := byte(rng.buffer)
	rng.buffer >>= 8
	return value
}

// u16 returns a pseudorandom 16bit int in range `[0, 2¹⁶)`.
[inline]
pub fn (mut rng WyRandRNG) u16() u16 {
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
pub fn (mut rng WyRandRNG) u32() u32 {
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
pub fn (mut rng WyRandRNG) u64() u64 {
	unsafe {
		mut seed1 := rng.state
		seed1 += wyrand.wyp0
		rng.state = seed1
		return hash.wymum(seed1 ^ wyrand.wyp1, seed1)
	}
	return 0
}

// block_size returns the number of bits that the RNG can produce in a single iteration.
[inline]
pub fn (mut rng WyRandRNG) block_size() int {
	return 64
}
