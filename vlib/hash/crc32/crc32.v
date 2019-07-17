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
const (
	Size = 4
)

// digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	crc 32
	table []u32
}

fn(d mut Digest) generate_table(poly int) {
	for i := 0; i < 256; i++ {
		mut crc := u32(i)
		for j := 0; j < 8; j++ {
			if crc&u32(1) == u32(1) {
				crc = u32((crc >> u32(1)) ^ poly)
			} else {
				crc >>= u32(1)
			}
		}
		d.table << crc
	}
}

fn(d &Digest) sum32(s string) u32 {
	return d.crc
}
 
fn (d &Digest) sum(s string) u32 {
	mut crc := ~u32(0)
	for i := 0; i < s.len; i++ {
		crc = d.table[byte(crc)^s[i]] ^ u32(crc >> u32(8))
	}
	return ~crc
}

pub fn(d &Digest) checksum(s string) u32 {
	return d.sum32(s)
}

// pass the polinomial to use
pub fn new(poly int) *Digest {
	mut d := &Digest{}
	d.reset()
	d.generate_table(poly)
	return d
}

// calculate crc32 using IEEE
pub fn sum(s string) u32 {
	mut c := new(IEEE)
	return c.sum32(s)
}

pub fn (d *Digest) Reset() { d.crc = 0 }

pub fn (d *Digest) Size() int { return Size }

pub fn (d *Digest) BlockSize() int { return 1 }
