// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package sha1 implements the SHA-1 hash algorithm as defined in RFC 3174.
// SHA-1 is cryptographically broken and should not be used for secure
// applications.
// Based off:   https://github.com/golang/go/blob/master/src/crypto/sha1
// Last commit: https://github.com/golang/go/commit/3ce865d7a0b88714cc433454ae2370a105210c01
module sha1

import encoding.binary

pub const (
	// The size of a SHA-1 checksum in bytes.
	size       = 20
	// The blocksize of SHA-1 in bytes.
	block_size = 64
)

const (
	chunk = 64
	init0 = 0x67452301
	init1 = 0xEFCDAB89
	init2 = 0x98BADCFE
	init3 = 0x10325476
	init4 = 0xC3D2E1F0
)

// digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	h   []u32
	x   []u8
	nx  int
	len u64
}

fn (mut d Digest) reset() {
	d.x = []u8{len: sha1.chunk}
	d.h = []u32{len: (5)}
	d.h[0] = u32(sha1.init0)
	d.h[1] = u32(sha1.init1)
	d.h[2] = u32(sha1.init2)
	d.h[3] = u32(sha1.init3)
	d.h[4] = u32(sha1.init4)
	d.nx = 0
	d.len = 0
}

// new returns a new Digest (implementing hash.Hash) computing the SHA1 checksum.
pub fn new() &Digest {
	mut d := &Digest{}
	d.reset()
	return d
}

// write writes the contents of `p_` to the internal hash representation.
[manualfree]
pub fn (mut d Digest) write(p_ []u8) ?int {
	nn := p_.len
	unsafe {
		mut p := p_
		d.len += u64(nn)
		if d.nx > 0 {
			n := copy(mut d.x[d.nx..], p)
			d.nx += n
			if d.nx == sha1.chunk {
				block(mut d, d.x)
				d.nx = 0
			}
			if n >= p.len {
				p = []
			} else {
				p = p[n..]
			}
		}
		if p.len >= sha1.chunk {
			n := p.len & ~(sha1.chunk - 1)
			block(mut d, p[..n])
			if n >= p.len {
				p = []
			} else {
				p = p[n..]
			}
		}
		if p.len > 0 {
			d.nx = copy(mut d.x, p)
		}
	}
	return nn
}

// sum returns a copy of the generated sum of the bytes in `b_in`.
pub fn (d &Digest) sum(b_in []u8) []u8 {
	// Make a copy of d so that caller can keep writing and summing.
	mut d0 := *d
	hash := d0.checksum()
	mut b_out := b_in.clone()
	for b in hash {
		b_out << b
	}
	return b_out
}

// checksum returns the current byte checksum of the `Digest`.
pub fn (mut d Digest) checksum() []u8 {
	mut len := d.len
	// Padding.  Add a 1 bit and 0 bits until 56 bytes mod 64.
	mut tmp := []u8{len: (64)}
	tmp[0] = 0x80
	if int(len) % 64 < 56 {
		d.write(tmp[..56 - int(len) % 64]) or { panic(err) }
	} else {
		d.write(tmp[..64 + 56 - int(len) % 64]) or { panic(err) }
	}
	// Length in bits.
	len <<= 3
	binary.big_endian_put_u64(mut tmp, len)
	d.write(tmp[..8]) or { panic(err) }
	mut digest := []u8{len: sha1.size}
	binary.big_endian_put_u32(mut digest, d.h[0])
	binary.big_endian_put_u32(mut digest[4..], d.h[1])
	binary.big_endian_put_u32(mut digest[8..], d.h[2])
	binary.big_endian_put_u32(mut digest[12..], d.h[3])
	binary.big_endian_put_u32(mut digest[16..], d.h[4])
	return digest
}

// sum returns the SHA-1 checksum of the bytes passed in `data`.
pub fn sum(data []u8) []u8 {
	mut d := new()
	d.write(data) or { panic(err) }
	return d.checksum()
}

fn block(mut dig Digest, p []u8) {
	// For now just use block_generic until we have specific
	// architecture optimized versions
	block_generic(mut dig, p)
}

// size returns the size of the checksum in bytes.
pub fn (d &Digest) size() int {
	return sha1.size
}

// block_size returns the block size of the checksum in bytes.
pub fn (d &Digest) block_size() int {
	return sha1.block_size
}

// hexhash returns a hexadecimal SHA1 hash sum `string` of `s`.
pub fn hexhash(s string) string {
	return sum(s.bytes()).hex()
}
