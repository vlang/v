// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
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
	x   []byte
	nx  int
	len u64
}

fn (mut d Digest) reset() {
	d.x = []byte{len: (chunk)}
	d.h = []u32{len: (5)}
	d.h[0] = u32(init0)
	d.h[1] = u32(init1)
	d.h[2] = u32(init2)
	d.h[3] = u32(init3)
	d.h[4] = u32(init4)
	d.nx = 0
	d.len = 0
}

// new returns a new Digest (implementing hash.Hash) computing the SHA1 checksum.
pub fn new() &Digest {
	mut d := &Digest{}
	d.reset()
	return d
}

[manualfree]
pub fn (mut d Digest) write(p_ []byte) int {
	nn := p_.len
	unsafe {
		mut p := p_
		d.len += u64(nn)
		if d.nx > 0 {
			n := copy(d.x[d.nx..], p)
			d.nx += n
			if d.nx == chunk {
				block(mut d, d.x)
				d.nx = 0
			}
			if n >= p.len {
				p = []
			} else {
				p = p[n..]
			}
		}
		if p.len >= chunk {
			n := p.len & ~(chunk - 1)
			block(mut d, p[..n])
			if n >= p.len {
				p = []
			} else {
				p = p[n..]
			}
		}
		if p.len > 0 {
			d.nx = copy(d.x, p)
		}
	}
	return nn
}

pub fn (d &Digest) sum(b_in []byte) []byte {
	// Make a copy of d so that caller can keep writing and summing.
	mut d0 := *d
	hash := d0.checksum()
	mut b_out := b_in.clone()
	for b in hash {
		b_out << b
	}
	return b_out
}

fn (mut d Digest) checksum() []byte {
	mut len := d.len
	// Padding.  Add a 1 bit and 0 bits until 56 bytes mod 64.
	mut tmp := []byte{len: (64)}
	tmp[0] = 0x80
	if int(len) % 64 < 56 {
		d.write(tmp[..56 - int(len) % 64])
	} else {
		d.write(tmp[..64 + 56 - int(len) % 64])
	}
	// Length in bits.
	len <<= 3
	binary.big_endian_put_u64(mut tmp, len)
	d.write(tmp[..8])
	mut digest := []byte{len: (size)}
	binary.big_endian_put_u32(mut digest, d.h[0])
	binary.big_endian_put_u32(mut digest[4..], d.h[1])
	binary.big_endian_put_u32(mut digest[8..], d.h[2])
	binary.big_endian_put_u32(mut digest[12..], d.h[3])
	binary.big_endian_put_u32(mut digest[16..], d.h[4])
	return digest
}

// Sum returns the SHA-1 checksum of the data.
pub fn sum(data []byte) []byte {
	mut d := new()
	d.write(data)
	return d.checksum()
}

fn block(mut dig Digest, p []byte) {
	// For now just use block_generic until we have specific
	// architecture optimized versions
	block_generic(mut dig, p)
}

pub fn (d &Digest) size() int {
	return size
}

pub fn (d &Digest) block_size() int {
	return block_size
}

pub fn hexhash(s string) string {
	return sum(s.bytes()).hex()
}
