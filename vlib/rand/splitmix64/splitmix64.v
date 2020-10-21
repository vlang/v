// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module splitmix64

import rand.util

// Ported from http://xoshiro.di.unimi.it/splitmix64.c
pub struct SplitMix64RNG {
mut:
	state     u64 = util.time_seed_64()
	has_extra bool
	extra     u32
}

// rng.seed(seed_data) sets the seed of the accepting SplitMix64RNG to the given data
// in little-endian format (i.e. lower 32 bits are in [0] and higher 32 bits in [1]).
pub fn (mut rng SplitMix64RNG) seed(seed_data []u32) {
	if seed_data.len != 2 {
		eprintln('SplitMix64RNG needs 2 32-bit unsigned integers as the seed.')
		exit(1)
	}
	rng.state = seed_data[0] | (u64(seed_data[1]) << 32)
	rng.has_extra = false
}

// rng.u32() updates the PRNG state and returns the next pseudorandom u32
[inline]
pub fn (mut rng SplitMix64RNG) u32() u32 {
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
pub fn (mut rng SplitMix64RNG) u64() u64 {
	rng.state += (0x9e3779b97f4a7c15)
	mut z := rng.state
	z = (z ^ ((z >> u64(30)))) * (0xbf58476d1ce4e5b9)
	z = (z ^ ((z >> u64(27)))) * (0x94d049bb133111eb)
	return z ^ (z >> (31))
}

// rng.u32n(bound) returns a pseudorandom u32 less than the bound
[inline]
pub fn (mut rng SplitMix64RNG) u32n(bound u32) u32 {
	// This function is kept similar to the u64 version
	if bound == 0 {
		eprintln('max must be non-zero')
		exit(1)
	}
	threshold := -bound % bound
	for {
		r := rng.u32()
		if r >= threshold {
			return r % bound
		}
	}
	return u32(0)
}

// rng.u64n(bound) returns a pseudorandom u64 less than the bound
[inline]
pub fn (mut rng SplitMix64RNG) u64n(bound u64) u64 {
	// See pcg32.v for explanation of comment. This algorithm
	// existed before the refactoring.
	if bound == 0 {
		eprintln('max must be non-zero')
		exit(1)
	}
	threshold := -bound % bound
	for {
		r := rng.u64()
		if r >= threshold {
			return r % bound
		}
	}
	return u64(0)
}

// rng.u32n(min, max) returns a pseudorandom u32 value that is guaranteed to be in [min, max)
[inline]
pub fn (mut rng SplitMix64RNG) u32_in_range(min u32, max u32) u32 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u32n(max - min)
}

// rng.u64n(min, max) returns a pseudorandom u64 value that is guaranteed to be in [min, max)
[inline]
pub fn (mut rng SplitMix64RNG) u64_in_range(min u64, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u64n(max - min)
}

// rng.int() returns a pseudorandom 32-bit int (which may be negative)
[inline]
pub fn (mut rng SplitMix64RNG) int() int {
	return int(rng.u32())
}

// rng.i64() returns a pseudorandom 64-bit i64 (which may be negative)
[inline]
pub fn (mut rng SplitMix64RNG) i64() i64 {
	return i64(rng.u64())
}

// rng.int31() returns a pseudorandom 31-bit int which is non-negative
[inline]
pub fn (mut rng SplitMix64RNG) int31() int {
	return int(rng.u32() & util.u31_mask) // Set the 32nd bit to 0.
}

// rng.int63() returns a pseudorandom 63-bit int which is non-negative
[inline]
pub fn (mut rng SplitMix64RNG) int63() i64 {
	return i64(rng.u64() & util.u63_mask) // Set the 64th bit to 0.
}

// rng.intn(max) returns a pseudorandom int that lies in [0, max)
[inline]
pub fn (mut rng SplitMix64RNG) intn(max int) int {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return int(rng.u32n(u32(max)))
}

// rng.i64n(max) returns a pseudorandom int that lies in [0, max)
[inline]
pub fn (mut rng SplitMix64RNG) i64n(max i64) i64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return i64(rng.u64n(u64(max)))
}

// rng.int_in_range(min, max) returns a pseudorandom int that lies in [min, max)
[inline]
pub fn (mut rng SplitMix64RNG) int_in_range(min int, max int) int {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	// This supports negative ranges like [-10, -5) because the difference is positive
	return min + rng.intn(max - min)
}

// rng.i64_in_range(min, max) returns a pseudorandom i64 that lies in [min, max)
[inline]
pub fn (mut rng SplitMix64RNG) i64_in_range(min i64, max i64) i64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.i64n(max - min)
}

// rng.f32() returns a pseudorandom f32 value between 0.0 (inclusive) and 1.0 (exclusive) i.e [0, 1)
[inline]
pub fn (mut rng SplitMix64RNG) f32() f32 {
	return f32(rng.u32()) / util.max_u32_as_f32
}

// rng.f64() returns a pseudorandom f64 value between 0.0 (inclusive) and 1.0 (exclusive) i.e [0, 1)
[inline]
pub fn (mut rng SplitMix64RNG) f64() f64 {
	return f64(rng.u64()) / util.max_u64_as_f64
}

// rng.f32n() returns a pseudorandom f32 value in [0, max)
[inline]
pub fn (mut rng SplitMix64RNG) f32n(max f32) f32 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f32() * max
}

// rng.f64n() returns a pseudorandom f64 value in [0, max)
[inline]
pub fn (mut rng SplitMix64RNG) f64n(max f64) f64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f64() * max
}

// rng.f32_in_range(min, max) returns a pseudorandom f32 that lies in [min, max)
[inline]
pub fn (mut rng SplitMix64RNG) f32_in_range(min f32, max f32) f32 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.f32n(max - min)
}

// rng.i64_in_range(min, max) returns a pseudorandom i64 that lies in [min, max)
[inline]
pub fn (mut rng SplitMix64RNG) f64_in_range(min f64, max f64) f64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.f64n(max - min)
}
