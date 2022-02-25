// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module splitmix64

import math
import rand.seed

pub const seed_len = 2

// SplitMix64RNG ported from http://xoshiro.di.unimi.it/splitmix64.c
pub struct SplitMix64RNG {
mut:
	state      u64 = seed.time_seed_64()
	bytes_left int
	buffer     u64
}

// seed sets the seed of the accepting SplitMix64RNG to the given data
// in little-endian format (i.e. lower 32 bits are in [0] and higher 32 bits in [1]).
pub fn (mut rng SplitMix64RNG) seed(seed_data []u32) {
	if seed_data.len != 2 {
		eprintln('SplitMix64RNG needs 2 32-bit unsigned integers as the seed.')
		exit(1)
	}
	rng.state = seed_data[0] | (u64(seed_data[1]) << 32)
	rng.bytes_left = 0
	rng.buffer = 0
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
[inline]
pub fn (mut rng SplitMix64RNG) byte() byte {
	// Can we extract a value from the buffer?
	if rng.bytes_left >= 1 {
		rng.bytes_left -= 1
		value := byte(rng.buffer)
		rng.buffer >>= 8
		return value
	}
	// Add a new value to the buffer
	rng.buffer = rng.internal_u64()
	rng.bytes_left = 7
	value := byte(rng.buffer)
	rng.buffer >>= 8
	return value
}

// bytes returns a buffer of `bytes_needed` random bytes.
[inline]
pub fn (mut rng SplitMix64RNG) bytes(bytes_needed int) ?[]byte {
	if bytes_needed < 0 {
		return error('can not read < 0 random bytes')
	}
	mut res := []byte{len: bytes_needed}

	rng.read(mut res)

	return res
}

// read fills up the buffer with random bytes.
pub fn (mut rng SplitMix64RNG) read(mut buf []byte) {
	mut bytes_needed := buf.len
	mut index := 0

	for _ in 0 .. math.min(rng.bytes_left, bytes_needed) {
		buf[index] = rng.byte()
		bytes_needed--
		index++
	}

	for bytes_needed >= 8 {
		mut full_value := rng.u64()
		for _ in 0 .. 8 {
			buf[index] = byte(full_value)
			full_value >>= 8
			index++
		}
		bytes_needed -= 8
	}

	for bytes_needed > 0 {
		buf[index] = rng.byte()
		index++
		bytes_needed--
	}
}

[inline]
fn (mut rng SplitMix64RNG) step_by(amount int) u64 {
	next_number := rng.internal_u64()

	bits_left := rng.bytes_left * 8
	bits_needed := amount - bits_left

	old_value := rng.buffer & ((u64(1) << bits_left) - 1)
	new_value := next_number & ((u64(1) << bits_needed) - 1)
	value := old_value | (new_value << bits_left)

	rng.buffer = next_number >> bits_needed
	rng.bytes_left = 8 - (bits_needed / 8)

	return value
}

// u16 returns a pseudorandom 16bit int in range `[0, 2¹⁶)`.
[inline]
pub fn (mut rng SplitMix64RNG) u16() u16 {
	// Can we take a whole u16 out of the buffer?
	if rng.bytes_left >= 2 {
		rng.bytes_left -= 2
		value := u16(rng.buffer)
		rng.buffer >>= 16
		return value
	}
	if rng.bytes_left > 0 {
		return u16(rng.step_by(16))
	}
	ans := rng.internal_u64()
	rng.buffer = ans >> 16
	rng.bytes_left = 6
	return u16(ans)
}

// u32 returns a pseudorandom 32bit int in range `[0, 2³²)`.
[inline]
pub fn (mut rng SplitMix64RNG) u32() u32 {
	// Can we take a whole u32 out of the buffer?
	if rng.bytes_left >= 4 {
		rng.bytes_left -= 4
		value := u32(rng.buffer)
		rng.buffer >>= 32
		return value
	}
	if rng.bytes_left > 0 {
		return u32(rng.step_by(32))
	}
	// We're out so we start fresh.
	ans := rng.internal_u64()
	rng.buffer = ans >> 32
	rng.bytes_left = 4
	return u32(ans)
}

// u64 returns a pseudorandom 64bit int in range `[0, 2⁶⁴)`.
[inline]
pub fn (mut rng SplitMix64RNG) u64() u64 {
	if rng.bytes_left > 0 {
		return rng.step_by(64)
	}
	return rng.internal_u64()
}

// u64 updates the PRNG state and returns the next pseudorandom `u64`.
[inline]
fn (mut rng SplitMix64RNG) internal_u64() u64 {
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
