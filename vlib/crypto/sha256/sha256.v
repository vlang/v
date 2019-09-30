// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Package sha256 implements the SHA224 and SHA256 hash algorithms as defined
// in FIPS 180-4.

// Based off:   https://github.com/golang/go/tree/master/src/crypto/sha256
// Last commit: https://github.com/golang/go/commit/3ce865d7a0b88714cc433454ae2370a105210c01

module sha256

import encoding.binary

const (
	// The size of a SHA256 checksum in bytes.
	Size = 32
	// The size of a SHA224 checksum in bytes.
	Size224 = 28
	// The blocksize of SHA256 and SHA224 in bytes.
	BlockSize = 64
)

const (
	Chunk     = 64
	Init0     = 0x6A09E667
	Init1     = 0xBB67AE85
	Init2     = 0x3C6EF372
	Init3     = 0xA54FF53A
	Init4     = 0x510E527F
	Init5     = 0x9B05688C
	Init6     = 0x1F83D9AB
	Init7     = 0x5BE0CD19
	Init0_224 = 0xC1059ED8
	Init1_224 = 0x367CD507
	Init2_224 = 0x3070DD17
	Init3_224 = 0xF70E5939
	Init4_224 = 0xFFC00B31
	Init5_224 = 0x68581511
	Init6_224 = 0x64F98FA7
	Init7_224 = 0xBEFA4FA4
)

// digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	h     []u32
	x     []byte
	nx    int
	len   u64
	is224 bool // mark if this digest is SHA-224
}

fn (d mut Digest) reset() {
	d.h = [u32(0)].repeat(8)
	d.x = [byte(0)].repeat(Chunk)
	if !d.is224 {
		d.h[0] = u32(Init0)
		d.h[1] = u32(Init1)
		d.h[2] = u32(Init2)
		d.h[3] = u32(Init3)
		d.h[4] = u32(Init4)
		d.h[5] = u32(Init5)
		d.h[6] = u32(Init6)
		d.h[7] = u32(Init7)
	} else {
		d.h[0] = u32(Init0_224)
		d.h[1] = u32(Init1_224)
		d.h[2] = u32(Init2_224)
		d.h[3] = u32(Init3_224)
		d.h[4] = u32(Init4_224)
		d.h[5] = u32(Init5_224)
		d.h[6] = u32(Init6_224)
		d.h[7] = u32(Init7_224)
	}
	d.nx = 0
	d.len = 0
}

// new returns a new Digest (implementing hash.Hash) computing the SHA256 checksum.
pub fn new() &Digest {
	mut d := &Digest{}
	d.reset()
	return d
}

// new224 returns a new Digest (implementing hash.Hash) computing the SHA224 checksum.
pub fn new224() &Digest {
	mut d := &Digest{}
	d.is224 = true
	d.reset()
	return d
}

fn (d mut Digest) write(p_ []byte) ?int {
	mut p := p_
	nn := p.len
	d.len += u64(nn)
	if d.nx > 0 {
		n := copy(d.x.right(d.nx), p)
		d.nx += n
		if d.nx == Chunk {
			block(mut d, d.x)
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
		block(mut d, p.left(n))
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

fn (d &Digest) sum(b_in []byte) []byte {
	// Make a copy of d so that caller can keep writing and summing.
	mut d0 := *d
	hash := d0.checksum()
	mut b_out := b_in.clone()
	if d0.is224 {
		for b in hash.left(Size224) {
			b_out << b
		}
	} else {
		for b in hash {
			b_out << b
		}
	}
	return b_out
}

fn (d mut Digest) checksum() []byte {
	mut len := d.len
	// Padding. Add a 1 bit and 0 bits until 56 bytes mod 64.
	mut tmp := [byte(0)].repeat(64)
	tmp[0] = 0x80
	if int(len)%64 < 56 {
		d.write(tmp.left(56-int(len)%64))
	} else {
		d.write(tmp.left(64+56-int(len)%64))
	}

	// Length in bits.
	len <<= u64(3)
	binary.big_endian_put_u64(mut tmp, len)
	d.write(tmp.left(8))

	if d.nx != 0 {
		panic('d.nx != 0')
	}

	digest := [byte(0)].repeat(Size)

	binary.big_endian_put_u32(mut digest, d.h[0])
	binary.big_endian_put_u32(mut digest.right(4), d.h[1])
	binary.big_endian_put_u32(mut digest.right(8), d.h[2])
	binary.big_endian_put_u32(mut digest.right(12), d.h[3])
	binary.big_endian_put_u32(mut digest.right(16), d.h[4])
	binary.big_endian_put_u32(mut digest.right(20), d.h[5])
	binary.big_endian_put_u32(mut digest.right(24), d.h[6])
	if !d.is224 {
		binary.big_endian_put_u32(mut digest.right(28), d.h[7])
	}

	return digest
}

// sum256 returns the SHA256 checksum of the data.
pub fn sum(data []byte) []byte {
	return sum256(data)
}

// sum256 returns the SHA256 checksum of the data.
pub fn sum256(data []byte) []byte {
	mut d := new()
	d.write(data)
	return d.checksum()
}

// sum224 returns the SHA224 checksum of the data.
pub fn sum224(data []byte) []byte {
	mut d := new224()
	d.write(data)
	sum := d.checksum()
	mut sum224 := [byte(0)].repeat(Size224)
	copy(sum224, sum.left(Size224))
	return sum224
}

fn block(dig mut Digest, p []byte) {
	// For now just use block_generic until we have specific
	// architecture optimized versions
	block_generic(mut dig, p)
}

pub fn (d &Digest) size() int {
	if !d.is224 {
		return Size
	}
	return Size224
}

pub fn (d &Digest) block_size() int { return BlockSize }

pub fn hexhash(s string) string { return sum256(s.bytes()).hex() }
pub fn hexhash_224(s string) string { return sum224(s.bytes()).hex() }
