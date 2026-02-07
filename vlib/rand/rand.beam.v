// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

import time

// BEAM backend implementation for rand module.
// Provides initialization and helper functions for the BEAM/Erlang target.

// init initializes the default RNG.
fn init() {
	default_rng = new_default()
}

// deinit is a no-op for BEAM (garbage collected)
fn deinit() {
	// No-op for BEAM - garbage collected
}

@[direct_array_access]
fn internal_string_from_set(mut rng PRNG, charset string, len int) string {
	if len == 0 {
		return ''
	}
	mut buf := []u8{len: len}
	for i in 0 .. len {
		buf[i] = charset[rng.u32() % u32(charset.len)]
	}
	return buf.bytestr()
}

@[direct_array_access]
fn internal_fill_buffer_from_set(mut rng PRNG, charset string, mut buf []u8) {
	if buf.len == 0 {
		return
	}
	blen := buf.len
	for i in 0 .. blen {
		buf[i] = charset[rng.u32() % u32(charset.len)]
	}
}

const ulid_encoding = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'

@[direct_array_access]
fn internal_ulid_at_millisecond(mut rng PRNG, unix_time_milli u64) string {
	mut buf := []u8{len: 26}
	mut t := unix_time_milli
	mut i := 9
	for i >= 0 {
		buf[i] = ulid_encoding[int(t & 0x1F)]
		t = t >> 5
		i--
	}
	// first rand set
	mut x := rng.u64()
	i = 10
	for i < 19 {
		buf[i] = ulid_encoding[int(x & 0x1F)]
		x = x >> 5
		i++
	}
	// second rand set
	x = rng.u64()
	for i < 26 {
		buf[i] = ulid_encoding[int(x & 0x1F)]
		x = x >> 5
		i++
	}
	return buf.bytestr()
}

fn read_internal(mut rng PRNG, mut buf []u8) {
	for i in 0 .. buf.len {
		buf[i] = rng.u8()
	}
}

// uuid_v4 generates a random (v4) UUID.
@[direct_array_access]
pub fn uuid_v4() string {
	rand_1 := default_rng.u64()
	rand_2 := default_rng.u64()
	return internal_uuid(4, rand_1, rand_2)
}

// uuid_v7 generates a time-ordered (v7) UUID.
pub fn uuid_v7() string {
	import_time := u64(time.now().unix_milli()) << 16
	rand_1 := import_time | default_rng.u16()
	rand_2 := default_rng.u64()
	return internal_uuid(7, rand_1, rand_2)
}

pub struct UUIDSession {
mut:
	counter u8
}

pub fn new_uuid_v7_session() UUIDSession {
	return UUIDSession{}
}

@[ignore_overflow]
pub fn (mut u UUIDSession) next() string {
	timestamp := u64(time.now().unix_nano())
	timestamp_shift_4bits := (timestamp & 0xFFFF_FFFF_FFFF_0000) | ((timestamp & 0x0000_0000_0000_FFFF) >> 4)
	rand_1 := (timestamp_shift_4bits & 0xFFFF_FFFF_FFFF_FFC0) | u64(u.counter & 0x3F)
	rand_2 := default_rng.u64()
	u.counter++
	return internal_uuid(7, rand_1, rand_2)
}

@[direct_array_access]
fn internal_uuid(version u8, rand_1 u64, rand_2 u64) string {
	mut parts := [8]u16{}
	parts[0] = u16(rand_1 >> 48)
	parts[1] = u16(rand_1 >> 32)
	parts[2] = u16(rand_1 >> 16)
	parts[3] = u16(rand_1)
	parts[4] = u16(rand_2 >> 48)
	parts[5] = u16(rand_2 >> 32)
	parts[6] = u16(rand_2 >> 16)
	parts[7] = u16(rand_2)

	parts[3] = (parts[3] & 0x0FFF) | (u16(version) << 12)
	parts[4] = (parts[4] & 0x3FFF) | 0x8000

	mut buf := []u8{len: 36}
	mut start := 0
	for i in 0 .. 8 {
		val := parts[i]
		buf[start] = hex_chars[(val >> 12) & 0xF]
		buf[start + 1] = hex_chars[(val >> 8) & 0xF]
		buf[start + 2] = hex_chars[(val >> 4) & 0xF]
		buf[start + 3] = hex_chars[val & 0xF]
		start += 4
		if start == 8 || start == 13 || start == 18 || start == 23 {
			buf[start] = `-`
			start++
		}
	}
	return buf.bytestr()
}
