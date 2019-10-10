// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Package sha1 implements the SHA-1 hash algorithm as defined in RFC 3174.

// SHA-1 is cryptographically broken and should not be used for secure
// applications.

// Based off:   https://github.com/golang/go/blob/master/src/crypto/sha1
// Last commit: https://github.com/golang/go/commit/3ce865d7a0b88714cc433454ae2370a105210c01 

module sha1

import encoding.binary

const(
	// The size of a SHA-1 checksum in bytes.
	Size     = 20
	// The blocksize of SHA-1 in bytes.
	BlockSize = 64
)

const (
	Chunk = 64
	Init0 = 0x67452301
	Init1 = 0xEFCDAB89
	Init2 = 0x98BADCFE
	Init3 = 0x10325476
	Init4 = 0xC3D2E1F0
)

// digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	h   []u32
	x   []byte
	nx  int
	len u64
}

fn (d mut Digest) reset() {
	d.x = [byte(0)].repeat(Chunk)
	d.h = [u32(0)].repeat(5)
	d.h[0] = u32(Init0)
	d.h[1] = u32(Init1)
	d.h[2] = u32(Init2)
	d.h[3] = u32(Init3)
	d.h[4] = u32(Init4)
	d.nx = 0
	d.len = 0
}

// new returns a new Digest (implementing hash.Hash) computing the SHA1 checksum.
pub fn new() &Digest {
	mut d := &Digest{}
	d.reset()
	return d
}

pub fn (d mut Digest) write(p_ []byte) ?int {
	mut p := p_
	nn := p.len
	d.len += u64(nn)

	if d.nx > 0 {
		n := copy(d.x.right(d.nx), p)
		d.nx += n
		if d.nx == Chunk {
			block(d, d.x)
			d.nx = 0
		}
		if n >= p.len {
			p = []byte
		} else {
			p = p.right(n)
		}
	}
	if p.len >= Chunk {
		n := p.len &~ (Chunk - 1)
		block(d, p.left(n))
		if n >= p.len {
			p = []byte
		} else {
			p = p.right(n)
		}
	}
	if p.len > 0 {
		d.nx = copy(d.x, p)
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

fn (d mut Digest) checksum() []byte {
	mut len := d.len
	// Padding.  Add a 1 bit and 0 bits until 56 bytes mod 64.
	mut tmp := [byte(0)].repeat(64)

	tmp[0] = 0x80

	if int(len)%64 < 56 {
		d.write(tmp.left(56-int(len)%64))
	} else {
		d.write(tmp.left(64+56-int(len)%64))
	}

	// Length in bits.
	len <<= 3
	binary.big_endian_put_u64(mut tmp, len)
	d.write(tmp.left(8))

	mut digest := [byte(0)].repeat(Size)

	binary.big_endian_put_u32(mut digest, d.h[0])
	binary.big_endian_put_u32(mut digest.right(4), d.h[1])
	binary.big_endian_put_u32(mut digest.right(8), d.h[2])
	binary.big_endian_put_u32(mut digest.right(12), d.h[3])
	binary.big_endian_put_u32(mut digest.right(16), d.h[4])

	return digest
}

// Sum returns the SHA-1 checksum of the data.
pub fn sum(data []byte) []byte {
	mut d := new()
	d.write(data)
	return d.checksum()
}

fn block(dig &Digest, p []byte) {
	// For now just use block_generic until we have specific
	// architecture optimized versions
	block_generic(mut dig, p)
}

pub fn (d &Digest) size() int { return Size }

pub fn (d &Digest) block_size() int { return BlockSize }

pub fn hexhash(s string) string { return sum(s.bytes()).hex() }
