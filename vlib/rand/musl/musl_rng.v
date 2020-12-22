// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module musl

import math.bits
import rand.util

// Ported from https://git.musl-libc.org/cgit/musl/tree/src/prng/rand_r.c
pub struct MuslRNG {
mut:
	state u32 = util.time_seed_32()
}

pub fn (mut rng MuslRNG) seed(seed_data []u32) {
	if seed_data.len != 1 {
		eprintln('MuslRNG needs only one unsigned 32 bit integer as a seed.')
		exit(1)
	}
	rng.state = seed_data[0]
}

[inline]
fn temper(prev u32) u32 {
	mut x := prev
	x ^= x >> 11
	x ^= (x << 7) & 0x9D2C5680
	x ^= (x << 15) & 0xEFC60000
	x ^= (x >> 18)
	return x
}

// rng.u32() - return a pseudorandom 32 bit unsigned u32
[inline]
pub fn (mut rng MuslRNG) u32() u32 {
	rng.state = rng.state * 1103515245 + 12345
	// We are not dividing by 2 (or shifting right by 1)
	// because we want all 32-bits of random data
	return temper(rng.state)
}

// rng.u64() - return a pseudorandom 64 bit unsigned u64
[inline]
pub fn (mut rng MuslRNG) u64() u64 {
	return u64(rng.u32()) | (u64(rng.u32()) << 32)
}

// rn.u32n(max) - return a pseudorandom 32 bit unsigned u32 in [0, max)
[inline]
pub fn (mut rng MuslRNG) u32n(max u32) u32 {
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

// rn.u64n(max) - return a pseudorandom 64 bit unsigned u64 in [0, max)
[inline]
pub fn (mut rng MuslRNG) u64n(max u64) u64 {
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

// rn.u32_in_range(min, max) - return a pseudorandom 32 bit unsigned u32 in [min, max)
[inline]
pub fn (mut rng MuslRNG) u32_in_range(min u64, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u32n(u32(max - min))
}

// rn.u64_in_range(min, max) - return a pseudorandom 64 bit unsigned u64 in [min, max)
[inline]
pub fn (mut rng MuslRNG) u64_in_range(min u64, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u64n(max - min)
}

// rng.int() - return a 32-bit signed (possibly negative) int
[inline]
pub fn (mut rng MuslRNG) int() int {
	return int(rng.u32())
}

// rng.i64() - return a 64-bit signed (possibly negative) i64
[inline]
pub fn (mut rng MuslRNG) i64() i64 {
	return i64(rng.u64())
}

// rng.int31() - return a 31bit positive pseudorandom integer
[inline]
pub fn (mut rng MuslRNG) int31() int {
	return int(rng.u32() >> 1)
}

// rng.int63() - return a 63bit positive pseudorandom integer
[inline]
pub fn (mut rng MuslRNG) int63() i64 {
	return i64(rng.u64() >> 1)
}

// rng.intn(max) - return a 32bit positive int in [0, max)
[inline]
pub fn (mut rng MuslRNG) intn(max int) int {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return int(rng.u32n(u32(max)))
}

// rng.i64n(max) - return a 64bit positive i64 in [0, max)
[inline]
pub fn (mut rng MuslRNG) i64n(max i64) i64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return i64(rng.u64n(u64(max)))
}

// rng.int_in_range(min, max) - return a 32bit positive int in [0, max)
[inline]
pub fn (mut rng MuslRNG) int_in_range(min int, max int) int {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.intn(max - min)
}

// rng.i64_in_range(min, max) - return a 64bit positive i64 in [0, max)
[inline]
pub fn (mut rng MuslRNG) i64_in_range(min i64, max i64) i64 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.i64n(max - min)
}

// rng.f32() returns a pseudorandom f32 value between 0.0 (inclusive) and 1.0 (exclusive) i.e [0, 1)
[inline]
pub fn (mut rng MuslRNG) f32() f32 {
	return f32(rng.u32()) / util.max_u32_as_f32
}

// rng.f64() returns a pseudorandom f64 value between 0.0 (inclusive) and 1.0 (exclusive) i.e [0, 1)
[inline]
pub fn (mut rng MuslRNG) f64() f64 {
	return f64(rng.u64()) / util.max_u64_as_f64
}

// rng.f32n() returns a pseudorandom f32 value in [0, max)
[inline]
pub fn (mut rng MuslRNG) f32n(max f32) f32 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f32() * max
}

// rng.f64n() returns a pseudorandom f64 value in [0, max)
[inline]
pub fn (mut rng MuslRNG) f64n(max f64) f64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f64() * max
}

// rng.f32_in_range(min, max) returns a pseudorandom f32 that lies in [min, max)
[inline]
pub fn (mut rng MuslRNG) f32_in_range(min f32, max f32) f32 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.f32n(max - min)
}

// rng.i64_in_range(min, max) returns a pseudorandom i64 that lies in [min, max)
[inline]
pub fn (mut rng MuslRNG) f64_in_range(min f64, max f64) f64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.f64n(max - min)
}
