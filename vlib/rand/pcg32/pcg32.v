// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pcg32

import rand.seed

pub const seed_len = 4

// PCG32RNG ported from http://www.pcg-random.org/download.html,
// https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c, and
// https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.h
pub struct PCG32RNG {
mut:
	state      u64 = u64(0x853c49e6748fea9b) ^ seed.time_seed_64()
	inc        u64 = u64(0xda3e39cb94b95bdb) ^ seed.time_seed_64()
	bytes_left int
	buffer     u32
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
	rng.bytes_left = 0
	rng.buffer = 0
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
[inline]
fn (mut rng PCG32RNG) byte() byte {
	if rng.bytes_left >= 1 {
		rng.bytes_left -= 1
		value := byte(rng.buffer)
		rng.buffer >>= 8
		return value
	}
	rng.buffer = rng.u32()
	rng.bytes_left = 3
	value := byte(rng.buffer)
	rng.buffer >>= 8
	return value
}

// u16 returns a pseudorandom 16-bit unsigned integer (`u16`).
[inline]
pub fn (mut rng PCG32RNG) u16() u16 {
	if rng.bytes_left >= 2 {
		rng.bytes_left -= 2
		value := u16(rng.buffer)
		rng.buffer >>= 16
		return value
	}
	ans := rng.u32()
	rng.buffer = ans >> 16
	rng.bytes_left = 2
	return u16(ans)
}

// u32 returns a pseudorandom unsigned `u32`.
[inline]
fn (mut rng PCG32RNG) u32() u32 {
	oldstate := rng.state
	rng.state = oldstate * (6364136223846793005) + rng.inc
	xorshifted := u32(((oldstate >> u64(18)) ^ oldstate) >> u64(27))
	rot := u32(oldstate >> u64(59))
	return (xorshifted >> rot) | (xorshifted << ((-rot) & u32(31)))
}

// u64 returns a pseudorandom 64-bit unsigned `u64`.
[inline]
pub fn (mut rng PCG32RNG) u64() u64 {
	return u64(rng.u32()) | (u64(rng.u32()) << 32)
}

// block_size returns the number of bits that the RNG can produce in a single iteration.
[inline]
pub fn (mut rng PCG32RNG) block_size() int {
	return 32
}

// free should be called when the generator is no longer needed
[unsafe]
pub fn (mut rng PCG32RNG) free() {
	unsafe { free(rng) }
}
