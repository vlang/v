// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This is a very basic crc32 implementation
// at the moment with no architecture optimizations
module crc32

// polynomials
pub const ieee = u32(0xedb88320)
pub const castagnoli = u32(0x82f63b78)
pub const koopman = u32(0xeb31d82e)

// The size of a CRC-32 checksum in bytes.
const size = 4

struct Crc32 {
mut:
	table []u32
}

// generate_table populates a 256-word table from the specified polynomial `poly`
// to represent the polynomial for efficient processing.
fn (mut c Crc32) generate_table(poly int) {
	for i in 0 .. 256 {
		mut crc := u32(i)
		for _ in 0 .. 8 {
			if crc & u32(1) == u32(1) {
				crc = (crc >> 1) ^ u32(poly)
			} else {
				crc >>= u32(1)
			}
		}
		c.table << crc
	}
}

@[direct_array_access]
fn (c &Crc32) sum32(b []u8) u32 {
	mut crc := ~u32(0)
	for i in 0 .. b.len {
		crc = c.table[u8(crc) ^ b[i]] ^ (crc >> 8)
	}
	return ~crc
}

// checksum returns the CRC-32 checksum of data `b` by using the polynomial represented by `c`'s table.
pub fn (c &Crc32) checksum(b []u8) u32 {
	return c.sum32(b)
}

// new creates a `Crc32` polynomial.
pub fn new(poly int) &Crc32 {
	mut c := &Crc32{}
	c.generate_table(poly)
	return c
}

const ieee_poly = new(int(ieee))

// sum calculates the CRC-32 checksum of `b` by using the IEEE polynomial.
pub fn sum(b []u8) u32 {
	return ieee_poly.sum32(b)
}
