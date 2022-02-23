// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wyrand

import rand.seed
import rand.constants
import hash

// Redefinition of some constants that we will need for pseudorandom number generation.
const (
	wyp0 = u64(0xa0761d6478bd642f)
	wyp1 = u64(0xe7037ed1a0b428db)
)

// WyRandRNG is a RNG based on the WyHash hashing algorithm.
pub struct WyRandRNG {
mut:
	state     u64 = seed.time_seed_64()
	has_extra bool
	extra     u32
}

// seed sets the seed, needs only two `u32`s in little-endian format as [lower, higher].
pub fn (mut rng WyRandRNG) seed(seed_data []u32) {
	if seed_data.len != 2 {
		eprintln('WyRandRNG needs 2 32-bit unsigned integers as the seed.')
		exit(1)
	}
	rng.state = seed_data[0] | (u64(seed_data[1]) << 32)
	rng.has_extra = false
}

// u32 updates the PRNG state and returns the next pseudorandom `u32`.
[inline]
pub fn (mut rng WyRandRNG) u32() u32 {
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
pub fn (mut rng WyRandRNG) u64() u64 {
	unsafe {
		mut seed1 := rng.state
		seed1 += wyrand.wyp0
		rng.state = seed1
		return hash.wymum(seed1 ^ wyrand.wyp1, seed1)
	}
	return 0
}
