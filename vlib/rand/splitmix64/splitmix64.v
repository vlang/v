// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module splitmix64

import rand.seed
import rand.constants

// SplitMix64RNG ported from http://xoshiro.di.unimi.it/splitmix64.c
pub struct SplitMix64RNG {
mut:
	state     u64 = seed.time_seed_64()
	has_extra bool
	extra     u32
}

// seed sets the seed of the accepting SplitMix64RNG to the given data
// in little-endian format (i.e. lower 32 bits are in [0] and higher 32 bits in [1]).
pub fn (mut rng SplitMix64RNG) seed(seed_data []u32) {
	if seed_data.len != 2 {
		eprintln('SplitMix64RNG needs 2 32-bit unsigned integers as the seed.')
		exit(1)
	}
	rng.state = seed_data[0] | (u64(seed_data[1]) << 32)
	rng.has_extra = false
}

// u32 updates the PRNG state and returns the next pseudorandom `u32`.
[inline]
pub fn (mut rng SplitMix64RNG) u32() u32 {
	if rng.has_extra {
		rng.has_extra = false
		return rng.extra
	}
	full_value := rng.u64()
	lower := u32(full_value & constants.lower_mask)
	upper := u32(full_value >> 32)
	rng.extra = upper
	rng.has_extra = true
	return lower
}

// u64 updates the PRNG state and returns the next pseudorandom `u64`.
[inline]
pub fn (mut rng SplitMix64RNG) u64() u64 {
	rng.state += (0x9e3779b97f4a7c15)
	mut z := rng.state
	z = (z ^ (z >> u64(30))) * 0xbf58476d1ce4e5b9
	z = (z ^ (z >> u64(27))) * 0x94d049bb133111eb
	return z ^ (z >> (31))
}

// free should be called when the generator is no longer needed
[unsafe]
pub fn (mut rng SplitMix64RNG) free() {
	unsafe { free(rng) }
}
