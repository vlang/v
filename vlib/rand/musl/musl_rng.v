// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module musl

import math.bits
import rand.util

// MuslRNG ported from https://git.musl-libc.org/cgit/musl/tree/src/prng/rand_r.c
pub struct MuslRNG {
mut:
	state u32 = util.time_seed_32()
}

// seed sets the current random state based on `seed_data`.
// seed expects `seed_data` to be only one `u32`.
pub fn (mut rng MuslRNG) seed(seed_data []u32) {
	if seed_data.len != 1 {
		eprintln('MuslRNG needs only one unsigned 32-bit integer as a seed.')
		exit(1)
	}
	rng.state = seed_data[0]
}

// temper returns a tempered value based on `prev` value.
[inline]
fn temper(prev u32) u32 {
	mut x := prev
	x ^= x >> 11
	x ^= (x << 7) & 0x9D2C5680
	x ^= (x << 15) & 0xEFC60000
	x ^= (x >> 18)
	return x
}

// u32 returns a pseudorandom 32-bit unsigned integer (`u32`).
[inline]
pub fn (mut rng MuslRNG) u32() u32 {
	rng.state = rng.state * 1103515245 + 12345
	// We are not dividing by 2 (or shifting right by 1)
	// because we want all 32-bits of random data
	return temper(rng.state)
}

// u64 returns a pseudorandom 64-bit unsigned integer (`u64`).
[inline]
pub fn (mut rng MuslRNG) u64() u64 {
	return u64(rng.u32()) | (u64(rng.u32()) << 32)
}

// u32n returns a pseudorandom 32-bit unsigned integer `u32` in range `[0, max)`.
[inline]
pub fn (mut rng MuslRNG) u32n(max u32) u32 {
	if max == 0 {
		eprintln('max must be positive integer.')
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

// u64n returns a pseudorandom 64-bit unsigned integer (`u64`) in range `[0, max)`.
[inline]
pub fn (mut rng MuslRNG) u64n(max u64) u64 {
	if max == 0 {
		eprintln('max must be positive integer.')
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

// u32_in_range returns a pseudorandom 32-bit unsigned integer (`u32`) in range `[min, max)`.
[inline]
pub fn (mut rng MuslRNG) u32_in_range(min u64, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.u32n(u32(max - min))
}

// u64_in_range returns a pseudorandom 64-bit unsigned integer (`u64`) in range `[min, max)`.
[inline]
pub fn (mut rng MuslRNG) u64_in_range(min u64, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.u64n(max - min)
}

// int returns a 32-bit signed (possibly negative) integer (`int`).
[inline]
pub fn (mut rng MuslRNG) int() int {
	return int(rng.u32())
}

// i64 returns a 64-bit signed (possibly negative) integer (`i64`).
[inline]
pub fn (mut rng MuslRNG) i64() i64 {
	return i64(rng.u64())
}

// int31 returns a 31-bit positive pseudorandom integer (`int`).
[inline]
pub fn (mut rng MuslRNG) int31() int {
	return int(rng.u32() >> 1)
}

// int63 returns a 63-bit positive pseudorandom integer (`i64`).
[inline]
pub fn (mut rng MuslRNG) int63() i64 {
	return i64(rng.u64() >> 1)
}

// intn returns a 32-bit positive int in range `[0, max)`.
[inline]
pub fn (mut rng MuslRNG) intn(max int) int {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return int(rng.u32n(u32(max)))
}

// i64n returns a 64-bit positive integer `i64` in range `[0, max)`.
[inline]
pub fn (mut rng MuslRNG) i64n(max i64) i64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return i64(rng.u64n(u64(max)))
}

// int_in_range returns a 32-bit positive integer `int` in range `[0, max)`.
[inline]
pub fn (mut rng MuslRNG) int_in_range(min int, max int) int {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.intn(max - min)
}

// i64_in_range returns a 64-bit positive integer `i64` in range `[0, max)`.
[inline]
pub fn (mut rng MuslRNG) i64_in_range(min i64, max i64) i64 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.i64n(max - min)
}

// f32 returns a pseudorandom `f32` value in range `[0, 1)`.
[inline]
pub fn (mut rng MuslRNG) f32() f32 {
	return f32(rng.u32()) / util.max_u32_as_f32
}

// f64 returns a pseudorandom `f64` value in range `[0, 1)`.
[inline]
pub fn (mut rng MuslRNG) f64() f64 {
	return f64(rng.u64()) / util.max_u64_as_f64
}

// f32n returns a pseudorandom `f32` value in range `[0, max)`.
[inline]
pub fn (mut rng MuslRNG) f32n(max f32) f32 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f32() * max
}

// f64n returns a pseudorandom `f64` value in range `[0, max)`.
[inline]
pub fn (mut rng MuslRNG) f64n(max f64) f64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f64() * max
}

// f32_in_range returns a pseudorandom `f32` in range `[min, max)`.
[inline]
pub fn (mut rng MuslRNG) f32_in_range(min f32, max f32) f32 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.f32n(max - min)
}

// i64_in_range returns a pseudorandom `i64` in range `[min, max)`.
[inline]
pub fn (mut rng MuslRNG) f64_in_range(min f64, max f64) f64 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.f64n(max - min)
}
