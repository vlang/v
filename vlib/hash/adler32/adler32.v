// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This is a pure V implementation of Adler-32 from RFC 1950.
// Parameters: BASE=65521, init=1, output=(s2 << 16) | s1.
module adler32

// base is the largest prime smaller than 2^16, mandated by RFC 1950.
pub const base = u32(65521)
// nmax is the largest chunk size that keeps intermediate sums inside u32.
// See RFC 1950 Appendix and zlib's adler32 implementation.
pub const nmax = 5552

// update_state updates an Adler-32 state with `data`.
// For RFC-1950 compliant checksums, use state `1` for a new stream.
@[direct_array_access]
pub fn update_state(state u32, data []u8) u32 {
	mut s1 := state & u32(0xffff)
	mut s2 := state >> 16
	mut pos := 0
	for pos < data.len {
		block_len := if data.len - pos > nmax { nmax } else { data.len - pos }
		for _ in 0 .. block_len {
			s1 += data[pos]
			if s1 >= base {
				s1 -= base
			}
			s2 += s1
			pos++
		}
		s2 %= base
	}
	return (s2 << 16) | s1
}

// checksum returns the RFC-1950 Adler-32 checksum for `data`.
pub fn checksum(data []u8) u32 {
	return update_state(u32(1), data)
}

// update extends an existing Adler-32 checksum `adler` with `data`.
// Use `adler = 1` for a fresh checksum.
pub fn update(adler u32, data []u8) u32 {
	return update_state(adler, data)
}

// sum is an alias for checksum.
pub fn sum(data []u8) u32 {
	return checksum(data)
}
