// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pcg32

import rand.seed
import rand.constants

// PCG32RNG ported from http://www.pcg-random.org/download.html,
// https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c, and
// https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.h
pub struct PCG32RNG {
mut:
	state u64 = u64(0x853c49e6748fea9b) ^ seed.time_seed_64()
	inc   u64 = u64(0xda3e39cb94b95bdb) ^ seed.time_seed_64()
}

// seed seeds the PCG32RNG with 4 `u32` values.
// The first 2 represent the 64-bit initial state as `[lower 32 bits, higher 32 bits]`
// The last 2 represent the 64-bit stream/step of the PRNG.
pub fn (mut rng PCG32RNG) seed(seed_data []u32) {
	if seed_data.len != 4 {
		eprintln('PCG32RNG needs 4 u32s to be seeded. First two the initial state and the last two the stream/step. Both in little endian format: [lower, higher].')
		exit(1)
	}
	init_state := u64(seed_data[0]) | (u64(seed_data[1]) << 32)
	init_seq := u64(seed_data[2]) | (u64(seed_data[3]) << 32)
	rng.state = u64(0)
	rng.inc = (init_seq << u64(1)) | u64(1)
	rng.u32()
	rng.state += init_state
	rng.u32()
}

// u32 returns a pseudorandom unsigned `u32`.
[inline]
pub fn (mut rng PCG32RNG) u32() u32 {
	oldstate := rng.state
	rng.state = oldstate * (6364136223846793005) + rng.inc
	xorshifted := u32(((oldstate >> u64(18)) ^ oldstate) >> u64(27))
	rot := u32(oldstate >> u64(59))
	return ((xorshifted >> rot) | (xorshifted << ((-rot) & u32(31))))
}

// u64 returns a pseudorandom 64-bit unsigned `u64`.
[inline]
pub fn (mut rng PCG32RNG) u64() u64 {
	return u64(rng.u32()) | (u64(rng.u32()) << 32)
}

// u32n returns a pseudorandom 32-bit unsigned `u32` in range `[0, max)`.
[inline]
pub fn (mut rng PCG32RNG) u32n(max u32) u32 {
	if max == 0 {
		eprintln('max must be positive')
		exit(1)
	}
	// To avoid bias, we need to make the range of the RNG a multiple of
	// max, which we do by dropping output less than a threshold.
	threshold := (-max % max)
	// Uniformity guarantees that loop below will terminate. In practice, it
	// should usually terminate quickly; on average (assuming all max's are
	// equally likely), 82.25% of the time, we can expect it to require just
	// one iteration. In practice, max's are typically small and only a
	// tiny amount of the range is eliminated.
	for {
		r := rng.u32()
		if r >= threshold {
			return (r % max)
		}
	}
	return u32(0)
}

// u64n returns a pseudorandom 64-bit unsigned `u64` in range `[0, max)`.
[inline]
pub fn (mut rng PCG32RNG) u64n(max u64) u64 {
	if max == 0 {
		eprintln('max must be positive')
		exit(1)
	}
	threshold := (-max % max)
	for {
		r := rng.u64()
		if r >= threshold {
			return (r % max)
		}
	}
	return u64(0)
}

// u32_in_range returns a pseudorandom 32-bit unsigned `u32` in range `[min, max)`.
[inline]
pub fn (mut rng PCG32RNG) u32_in_range(min u64, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u32n(u32(max - min))
}

// u64_in_range returns a pseudorandom 64-bit unsigned `u64` in range `[min, max)`.
[inline]
pub fn (mut rng PCG32RNG) u64_in_range(min u64, max u64) u64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.u64n(max - min)
}

// int returns a 32-bit signed (possibly negative) `int`.
[inline]
pub fn (mut rng PCG32RNG) int() int {
	return int(rng.u32())
}

// i64 returns a 64-bit signed (possibly negative) `i64`.
[inline]
pub fn (mut rng PCG32RNG) i64() i64 {
	return i64(rng.u64())
}

// int31 returns a 31-bit positive pseudorandom `int`.
[inline]
pub fn (mut rng PCG32RNG) int31() int {
	return int(rng.u32() >> 1)
}

// int63 returns a 63-bit positive pseudorandom `i64`.
[inline]
pub fn (mut rng PCG32RNG) int63() i64 {
	return i64(rng.u64() >> 1)
}

// intn returns a 32-bit positive `int` in range `[0, max)`.
[inline]
pub fn (mut rng PCG32RNG) intn(max int) int {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return int(rng.u32n(u32(max)))
}

// i64n returns a 64-bit positive `i64` in range `[0, max)`.
[inline]
pub fn (mut rng PCG32RNG) i64n(max i64) i64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return i64(rng.u64n(u64(max)))
}

// int_in_range returns a 32-bit positive `int` in range `[0, max)`.
[inline]
pub fn (mut rng PCG32RNG) int_in_range(min int, max int) int {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.intn(max - min)
}

// i64_in_range returns a 64-bit positive `i64` in range `[0, max)`.
[inline]
pub fn (mut rng PCG32RNG) i64_in_range(min i64, max i64) i64 {
	if max <= min {
		eprintln('max must be greater than min.')
		exit(1)
	}
	return min + rng.i64n(max - min)
}

// f32 returns a pseudorandom `f32` value in range `[0, 1)`.
[inline]
pub fn (mut rng PCG32RNG) f32() f32 {
	return f32(rng.u32()) / constants.max_u32_as_f32
}

// f64 returns a pseudorandom `f64` value in range `[0, 1)`.
[inline]
pub fn (mut rng PCG32RNG) f64() f64 {
	return f64(rng.u64()) / constants.max_u64_as_f64
}

// f32n returns a pseudorandom `f32` value in range `[0, max)`.
[inline]
pub fn (mut rng PCG32RNG) f32n(max f32) f32 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f32() * max
}

// f64n returns a pseudorandom `f64` value in range `[0, max)`.
[inline]
pub fn (mut rng PCG32RNG) f64n(max f64) f64 {
	if max <= 0 {
		eprintln('max has to be positive.')
		exit(1)
	}
	return rng.f64() * max
}

// f32_in_range returns a pseudorandom `f32` in range `[min, max)`.
[inline]
pub fn (mut rng PCG32RNG) f32_in_range(min f32, max f32) f32 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.f32n(max - min)
}

// i64_in_range returns a pseudorandom `i64` in range `[min, max)`.
[inline]
pub fn (mut rng PCG32RNG) f64_in_range(min f64, max f64) f64 {
	if max <= min {
		eprintln('max must be greater than min')
		exit(1)
	}
	return min + rng.f64n(max - min)
}
