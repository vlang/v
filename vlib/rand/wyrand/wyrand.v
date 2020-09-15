// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wyrand

import math.bits
import rand.util
import hash as wyhash

// Redefinition of some constants that we will need for pseudorandom number generation
const (
	wyp0 = u64(0xa0761d6478bd642f)
	wyp1 = u64(0xe7037ed1a0b428db)
)

// RNG based on the WyHash hashing algorithm
pub struct WyRandRNG {
mut:
	state     u64 = util.time_seed_64()
	has_extra bool
	extra     u32
}

// seed() - Set the seed, needs only two u32s in little endian format as [lower, higher]
pub fn (mut rng WyRandRNG) seed(seed_data []u32) {
	if seed_data.len != 2 {
		eprintln('WyRandRNG needs 2 32-bit unsigned integers as the seed.')
		exit(1)
	}
	rng.state = seed_data[0] | (u64(seed_data[1]) << 32)
	rng.has_extra = false
}

// rng.u32() updates the PRNG state and returns the next pseudorandom u32
[inline]
pub fn (mut rng WyRandRNG) u32() u32 {
	if rng.has_extra {
		rng.has_extra = false
		return rng.extra
	}
	full_value := rng.u64()
	lower := u32(full_value & util.lower_mask)
	upper := u32(full_value >> 32)
	rng.extra = upper
	rng.has_extra = true
	return lower
}

// rng.u64() updates the PRNG state and returns the next pseudorandom u64
[inline]
pub fn (mut rng WyRandRNG) u64() u64 {
	unsafe {
		mut seed1 := rng.state
		seed1 += wyp0
		rng.state = seed1
		return wyhash.wymum(seed1 ^ wyp1, seed1)
	}
	return 0
}

// rng.u32n(max) returns a pseudorandom u32 less than the max
[inline]
pub fn (mut rng WyRandRNG) u32n(max u32) u32 {
	if max == 0 {
		eprintln('max must be positive integer')
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

// rng.u64n(max) returns a pseudorandom u64 less than the max
[inline]
pub fn (mut rng WyRandRNG) u64n(max u64) u64 {
	if max == 0 {
		eprintln('max must be positive integer')
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

// rng.u32n(min, max) returns a pseudorandom u32 value that is guaranteed to be in [min, max)
[inline]
pub fn (mut rng WyRandRNG) u32_in_range(min, max u32) u32 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u32n(max - min)
}

// rng.u64n(min, max) returns a pseudorandom u64 value that is guaranteed to be in [min, max)
[inline]
pub fn (mut rng WyRandRNG) u64_in_range(min, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u64n(max - min)
}

// rng.int() returns a pseudorandom 32-bit int (which may be negative)
[inline]
pub fn (mut rng WyRandRNG) int() int {
	return int(rng.u32())
}

// rng.i64() returns a pseudorandom 64-bit i64 (which may be negative)
[inline]
pub fn (mut rng WyRandRNG) i64() i64 {
	return i64(rng.u64())
}

// rng.int31() returns a pseudorandom 31-bit int which is non-negative
[inline]
pub fn (mut rng WyRandRNG) int31() int {
	return int(rng.u32() & util.u31_mask) // Set the 32nd bit to 0.
}

// rng.int63() returns a pseudorandom 63-bit int which is non-negative
[inline]
pub fn (mut rng WyRandRNG) int63() i64 {
	return i64(rng.u64() & util.u63_mask) // Set the 64th bit to 0.
}

// rng.intn(max) returns a pseudorandom int that lies in [0, max)
[inline]
pub fn (mut rng WyRandRNG) intn(max int) int {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return int(rng.u32n(u32(max)))
}

// rng.i64n(max) returns a pseudorandom int that lies in [0, max)
[inline]
pub fn (mut rng WyRandRNG) i64n(max i64) i64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return i64(rng.u64n(u64(max)))
}

// rng.int_in_range(min, max) returns a pseudorandom int that lies in [min, max)
[inline]
pub fn (mut rng WyRandRNG) int_in_range(min, max int) int {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	// This supports negative ranges like [-10, -5) because the difference is positive
	return min + rng.intn(max - min)
}

// rng.i64_in_range(min, max) returns a pseudorandom i64 that lies in [min, max)
[inline]
pub fn (mut rng WyRandRNG) i64_in_range(min, max i64) i64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.i64n(max - min)
}

// rng.f32() returns a pseudorandom f32 value between 0.0 (inclusive) and 1.0 (exclusive) i.e [0, 1)
[inline]
pub fn (mut rng WyRandRNG) f32() f32 {
	return f32(rng.u32()) / util.max_u32_as_f32
}

// rng.f64() returns a pseudorandom f64 value between 0.0 (inclusive) and 1.0 (exclusive) i.e [0, 1)
[inline]
pub fn (mut rng WyRandRNG) f64() f64 {
	return f64(rng.u64()) / util.max_u64_as_f64
}

// rng.f32n() returns a pseudorandom f32 value in [0, max)
[inline]
pub fn (mut rng WyRandRNG) f32n(max f32) f32 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f32() * max
}

// rng.f64n() returns a pseudorandom f64 value in [0, max)
[inline]
pub fn (mut rng WyRandRNG) f64n(max f64) f64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f64() * max
}

// rng.f32_in_range(min, max) returns a pseudorandom f32 that lies in [min, max)
[inline]
pub fn (mut rng WyRandRNG) f32_in_range(min, max f32) f32 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.f32n(max - min)
}

// rng.i64_in_range(min, max) returns a pseudorandom i64 that lies in [min, max)
[inline]
pub fn (mut rng WyRandRNG) f64_in_range(min, max f64) f64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.f64n(max - min)
}
