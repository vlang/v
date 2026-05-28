// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This is a pure V implementation of CRC64, providing one standard variant:
// CRC-64-ECMA-182 (poly 0x42F0E1EBA9EA3693, init 0, refin false, refout false, xorout 0).
module crc64

// Polynomial constants for CRC-64
pub const ecma = u64(0x42F0E1EBA9EA3693)

struct Crc64 {
mut:
	table []u64
}

// generate_table populates a 256-word MSB-first lookup table for `poly`.
@[direct_array_access]
fn (mut c Crc64) generate_table(poly u64) {
	c.table = []u64{len: 256}
	for i in 0 .. 256 {
		mut crc := u64(i) << 56
		for _ in 0 .. 8 {
			if crc & (u64(1) << 63) != 0 {
				crc = (crc << 1) ^ poly
			} else {
				crc <<= 1
			}
		}
		c.table[i] = crc
	}
}

@[direct_array_access]
fn (c &Crc64) update64(crc u64, b []u8) u64 {
	mut next := crc
	for i in 0 .. b.len {
		next = c.table[u8(next >> 56) ^ b[i]] ^ (next << 8)
	}
	return next
}

// update_state updates an internal CRC state with the bytes in `b`.
// For CRC-64-ECMA-182, use state 0 for a new stream.
pub fn (c &Crc64) update_state(state u64, b []u8) u64 {
	return c.update64(state, b)
}

// checksum returns the CRC-64 checksum of data `b` by using the polynomial represented by `c`'s table.
pub fn (c &Crc64) checksum(b []u8) u64 {
	return c.update_state(u64(0), b)
}

// update returns the updated CRC-64 checksum for `b`, starting from `crc`.
// Use `crc = 0` for a fresh checksum, or pass a previous result to continue streaming.
pub fn (c &Crc64) update(crc u64, b []u8) u64 {
	return c.update_state(crc, b)
}

// new creates a `Crc64` polynomial.
pub fn new(poly u64) &Crc64 {
	mut c := &Crc64{}
	c.generate_table(poly)
	return c
}

// sum_with_poly calculates the CRC-64 checksum of `b` for the provided polynomial.
pub fn sum_with_poly(poly u64, b []u8) u64 {
	return match poly {
		ecma { ecma_poly.checksum(b) }
		else { new(poly).checksum(b) }
	}
}

const ecma_poly = new(ecma)

// sum calculates the CRC-64 checksum of `b` by using the ECMA polynomial.
pub fn sum(b []u8) u64 {
	return ecma_poly.checksum(b)
}
