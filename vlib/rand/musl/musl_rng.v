// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module musl

import rand.seed

pub const seed_len = 1

// MuslRNG ported from https://git.musl-libc.org/cgit/musl/tree/src/prng/rand_r.c
pub struct MuslRNG {
mut:
	state      u32 = seed.time_seed_32()
	bytes_left int
	buffer     u32
}

// seed sets the current random state based on `seed_data`.
// seed expects `seed_data` to be only one `u32`.
pub fn (mut rng MuslRNG) seed(seed_data []u32) {
	if seed_data.len != 1 {
		eprintln('MuslRNG needs only one unsigned 32-bit integer as a seed.')
		exit(1)
	}
	rng.state = seed_data[0]
	rng.bytes_left = 0
	rng.buffer = 0
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
[inline]
fn (mut rng MuslRNG) byte() byte {
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
pub fn (mut rng MuslRNG) u16() u16 {
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
fn (mut rng MuslRNG) u32() u32 {
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

// block_size returns the number of bits that the RNG can produce in a single iteration.
[inline]
pub fn (mut rng MuslRNG) block_size() int {
	return 32
}

// free should be called when the generator is no longer needed
[unsafe]
pub fn (mut rng MuslRNG) free() {
	unsafe { free(rng) }
}
