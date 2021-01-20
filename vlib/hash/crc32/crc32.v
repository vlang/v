// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This is a very basic crc32 implementation
// at the moment with no architecture optimizations
module crc32

// polynomials
pub const (
	ieee       = u32(0xedb88320)
	castagnoli = u32(0x82f63b78)
	koopman    = u32(0xeb31d82e)
)

// The size of a CRC-32 checksum in bytes.
const (
	size = 4
)

struct Crc32 {
mut:
	table []u32
}

fn(mut c Crc32) generate_table(poly int) {
	for i in 0..256 {
		mut crc := u32(i)
		for _ in 0..8 {
			if crc & u32(1) == u32(1) {
				crc = (crc >> 1) ^ u32(poly)
			} else {
				crc >>= u32(1)
			}
		}
		c.table << crc
	}
}

fn(c &Crc32) sum32(b []byte) u32 {
	mut crc := ~u32(0)
	for i in 0..b.len {
		crc = c.table[byte(crc)^b[i]] ^ (crc >> 8)
	}
	return ~crc
}

pub fn(c &Crc32) checksum(b []byte) u32 {
	return c.sum32(b)
}

// pass the polinomial to use
pub fn new(poly int) &Crc32 {
	mut c := &Crc32{}
	c.generate_table(poly)
	return c
}

// calculate crc32 using ieee
pub fn sum(b []byte) u32 {
	c := new(int(ieee))
	return c.sum32(b)
}
