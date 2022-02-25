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
	// Can we extract a value from the buffer?
	if rng.bytes_left >= 1 {
		rng.bytes_left -= 1
		value := byte(rng.buffer)
		rng.buffer >>= 8
		return value
	}
	// Add a new value to the buffer
	rng.buffer = rng.internal_u32()
	rng.bytes_left = 3
	value := byte(rng.buffer)
	rng.buffer >>= 8
	return value
}

// bytes returns a buffer of `bytes_needed` random bytes.
[inline]
pub fn (mut rng MuslRNG) bytes(bytes_needed int) ?[]byte {
	if bytes_needed < 0 {
		return error('can not read < 0 random bytes')
	}
	mut res := []byte{len: bytes_needed}

	rng.read(mut res)

	return res
}

// read fills up the buffer with random bytes.
pub fn (mut rng MuslRNG) read(mut buf []byte) {
	mut bytes_needed := buf.len
	mut index := 0

	for _ in 0 .. rng.bytes_left {
		buf[index] = rng.byte()
		bytes_needed--
		index++
	}

	for bytes_needed >= 4 {
		mut full_value := rng.u32()
		for _ in 0 .. 4 {
			buf[index] = byte(full_value)
			full_value >>= 8
			index++
		}
		bytes_needed -= 4
	}

	for bytes_needed > 0 {
		buf[index] += rng.byte()
		index++
		bytes_needed--
	}
}

[inline]
fn (mut rng MuslRNG) step_by(amount int) u32 {
	next_number := rng.internal_u32()

	bits_left := rng.bytes_left * 8
	bits_needed := amount - bits_left

	old_value := rng.buffer & ((u32(1) << bits_left) - 1)
	new_value := next_number & ((u32(1) << bits_needed) - 1)
	value := old_value | (new_value << bits_left)

	rng.buffer = next_number >> bits_needed
	rng.bytes_left = 4 - (bits_needed / 8)

	return value
}

// u16 returns a pseudorandom 16-bit unsigned integer (`u16`).
[inline]
pub fn (mut rng MuslRNG) u16() u16 {
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
	ans := rng.internal_u32()
	rng.buffer = ans >> 16
	rng.bytes_left = 2
	return u16(ans)
}

// u32 returns a pseudorandom 32-bit unsigned integer (`u32`).
[inline]
pub fn (mut rng MuslRNG) u32() u32 {
	if rng.bytes_left >= 1 {
		return rng.step_by(32)
	}
	return rng.internal_u32()
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

fn (mut rng MuslRNG) internal_u32() u32 {
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

// free should be called when the generator is no longer needed
[unsafe]
pub fn (mut rng MuslRNG) free() {
	unsafe { free(rng) }
}
