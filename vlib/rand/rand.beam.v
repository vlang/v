// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

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
