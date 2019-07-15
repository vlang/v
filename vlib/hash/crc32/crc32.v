// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This is a very basic crc32 implementation
// at the moment with no architecture optimizations
module crc32

// polynomials
const (
	IEEE       = 0xedb88320
	Castagnoli = 0x82f63b78
	Koopman    = 0xeb31d82e
)

// The size of a CRC-32 checksum in bytes.
const Size = 4

struct Crc32 {
mut:
	table []u32
}

struct Digest  {
	crc uint32
	tab *Table
}

func (d *igest) Reset() { d.crc = 0 }

fn(c mut Crc32) generate_table(poly int) {
	for i := 0; i < 256; i++ {
		mut crc := u32(i)
		for j := 0; j < 8; j++ {
			if crc&u32(1) == u32(1) {
				crc = u32((crc >> u32(1)) ^ poly)
			} else {
				crc >>= u32(1)
			}
		}
		c.table << crc
	}
}
 
fn(c &Crc32) sum32(s string) u32 {
	mut crc := ~u32(0)
	for i := 0; i < s.len; i++ {
		crc = c.table[byte(crc)^s[i]] ^ u32(crc >> u32(8))
	}
	return ~crc
}

pub fn(c &Crc32) checksum(s string) u32 {
	return c.sum_32(s)
}

// pass the polinomial to use
pub fn new(poly int) *Crc32 {
	mut c := &Crc32{}
	c.generate_table(poly)
	return c
}

// calculate crc32 using IEEE
pub fn sum(s string) u32 {
	mut c := new(IEEE)
	return c.sum32(s)
}

func (d *Digest) Size() int { return Size }

func (d *Digest) BlockSize() int { return 1 }