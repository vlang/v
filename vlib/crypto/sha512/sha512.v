// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Package sha512 implements the SHA-384, SHA-512, SHA-512/224, and SHA-512/256
// hash algorithms as defined in FIPS 180-4.

// Based off:   https://github.com/golang/go/tree/master/src/crypto/sha512
// Last commit: https://github.com/golang/go/commit/3ce865d7a0b88714cc433454ae2370a105210c01

module sha512

import (
	crypto
	encoding.binary
)

const (
	// Size is the size, in bytes, of a SHA-512 checksum.
	Size = 64
	// Size224 is the size, in bytes, of a SHA-512/224 checksum.
	Size224 = 28
	// Size256 is the size, in bytes, of a SHA-512/256 checksum.
	Size256 = 32
	// Size384 is the size, in bytes, of a SHA-384 checksum.
	Size384 = 48
	// BlockSize is the block size, in bytes, of the SHA-512/224,
	// SHA-512/256, SHA-384 and SHA-512 hash functions.
	BlockSize = 128
)

const (
	Chunk     = 128
	Init0     = 0x6a09e667f3bcc908
	Init1     = 0xbb67ae8584caa73b
	Init2     = 0x3c6ef372fe94f82b
	Init3     = 0xa54ff53a5f1d36f1
	Init4     = 0x510e527fade682d1
	Init5     = 0x9b05688c2b3e6c1f
	Init6     = 0x1f83d9abfb41bd6b
	Init7     = 0x5be0cd19137e2179
	Init0_224 = 0x8c3d37c819544da2
	Init1_224 = 0x73e1996689dcd4d6
	Init2_224 = 0x1dfab7ae32ff9c82
	Init3_224 = 0x679dd514582f9fcf
	Init4_224 = 0x0f6d2b697bd44da8
	Init5_224 = 0x77e36f7304c48942
	Init6_224 = 0x3f9d85a86a1d36c8
	Init7_224 = 0x1112e6ad91d692a1
	Init0_256 = 0x22312194fc2bf72c
	Init1_256 = 0x9f555fa3c84c64c2
	Init2_256 = 0x2393b86b6f53b151
	Init3_256 = 0x963877195940eabd
	Init4_256 = 0x96283ee2a88effe3
	Init5_256 = 0xbe5e1e2553863992
	Init6_256 = 0x2b0199fc2c85b8aa
	Init7_256 = 0x0eb72ddc81c52ca2
	Init0_384 = 0xcbbb9d5dc1059ed8
	Init1_384 = 0x629a292a367cd507
	Init2_384 = 0x9159015a3070dd17
	Init3_384 = 0x152fecd8f70e5939
	Init4_384 = 0x67332667ffc00b31
	Init5_384 = 0x8eb44a8768581511
	Init6_384 = 0xdb0c2e0d64f98fa7
	Init7_384 = 0x47b5481dbefa4fa4
)

// digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	h        []u64
	x        []byte
	nx       int
	len      u64
	function crypto.Hash
}

fn (d mut Digest) reset() {
	d.h = [u64(0)].repeat(8)
	d.x = [byte(0)].repeat(Chunk)
	switch d.function {
	case crypto.Hash.SHA384:
		d.h[0] = Init0_384
		d.h[1] = Init1_384
		d.h[2] = Init2_384
		d.h[3] = Init3_384
		d.h[4] = Init4_384
		d.h[5] = Init5_384
		d.h[6] = Init6_384
		d.h[7] = Init7_384
	case crypto.Hash.SHA512_224:
		d.h[0] = Init0_224
		d.h[1] = Init1_224
		d.h[2] = Init2_224
		d.h[3] = Init3_224
		d.h[4] = Init4_224
		d.h[5] = Init5_224
		d.h[6] = Init6_224
		d.h[7] = Init7_224
	case crypto.Hash.SHA512_256:
		d.h[0] = Init0_256
		d.h[1] = Init1_256
		d.h[2] = Init2_256
		d.h[3] = Init3_256
		d.h[4] = Init4_256
		d.h[5] = Init5_256
		d.h[6] = Init6_256
		d.h[7] = Init7_256
	default:
		d.h[0] = Init0
		d.h[1] = Init1
		d.h[2] = Init2
		d.h[3] = Init3
		d.h[4] = Init4
		d.h[5] = Init5
		d.h[6] = Init6
		d.h[7] = Init7
	}
	d.nx = 0
	d.len = 0
}

fn _new(hash crypto.Hash) &Digest {
	mut d := &Digest{function: hash}
	d.reset()
	return d
}

// new returns a new Digest (implementing hash.Hash) computing the SHA-512 checksum.
pub fn new() &Digest {
	return _new(crypto.Hash.SHA512)
}

// new512_224 returns a new Digest (implementing hash.Hash) computing the SHA-512/224 checksum.
fn new512_224() &Digest {
	return _new(crypto.Hash.SHA512_224)
}

// new512_256 returns a new Digest (implementing hash.Hash) computing the SHA-512/256 checksum.
fn new512_256() &Digest {
	return _new(crypto.Hash.SHA512_256)
}

// new384 returns a new Digest (implementing hash.Hash) computing the SHA-384 checksum.
fn new384() &Digest {
	return _new(crypto.Hash.SHA384)
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

fn (d mut Digest) sum(b_in []byte) []byte {
	// Make a copy of d so that caller can keep writing and summing.
	mut d0 := *d
	hash := d0.checksum()
	mut b_out := b_in.clone()
	switch d0.function {
	case crypto.Hash.SHA384:
		for b in hash.left(Size384) {
			b_out << b
		}
	case crypto.Hash.SHA512_224:
		for b in hash.left(Size224) {
			b_out << b
		}
	case crypto.Hash.SHA512_256:
		for b in hash.left(Size256) {
			b_out << b
		}
	default:
		for b in hash {
			b_out << b
		}
	}
	return b_out
}

fn (d mut Digest) checksum() []byte {
	// Padding. Add a 1 bit and 0 bits until 112 bytes mod 128.
	mut len := d.len
	mut tmp := [byte(0)].repeat(128)
	tmp[0] = 0x80

	if int(len)%128 < 112 {
		d.write(tmp.left(112-int(len)%128))
	} else {
		d.write(tmp.left(128+112-int(len)%128))
	}

	// Length in bits.
	len <<= u64(3)

	binary.big_endian_put_u64(mut tmp, u64(0)) // upper 64 bits are always zero, because len variable has type u64
	binary.big_endian_put_u64(mut tmp.right(8), len)
	d.write(tmp.left(16))

	if d.nx != 0 {
		panic('d.nx != 0')
	}

	mut digest := [byte(0)].repeat(Size)
	
	binary.big_endian_put_u64(mut digest, d.h[0])
	binary.big_endian_put_u64(mut digest.right(8), d.h[1])
	binary.big_endian_put_u64(mut digest.right(16), d.h[2])
	binary.big_endian_put_u64(mut digest.right(24), d.h[3])
	binary.big_endian_put_u64(mut digest.right(32), d.h[4])
	binary.big_endian_put_u64(mut digest.right(40), d.h[5])
	if d.function != crypto.Hash.SHA384 {
		binary.big_endian_put_u64(mut digest.right(48), d.h[6])
		binary.big_endian_put_u64(mut digest.right(56), d.h[7])
	}

	return digest
}

// sum512 returns the SHA512 checksum of the data.
pub fn sum512(data []byte) []byte {
	mut d := _new(crypto.Hash.SHA512)
	d.write(data)
	return d.checksum()
}

// sum384 returns the SHA384 checksum of the data.
pub fn sum384(data []byte) []byte {
	mut d := _new(crypto.Hash.SHA384)
	d.write(data)
	sum := d.checksum()
	mut sum384 := [byte(0)].repeat(Size384)
	copy(sum384, sum.left(Size384))
	return sum384
}

// sum512_224 returns the Sum512/224 checksum of the data.
pub fn sum512_224(data []byte) []byte {
	mut d := _new(crypto.Hash.SHA512_224)
	d.write(data)
	sum := d.checksum()
	mut sum224 := [byte(0)].repeat(Size224)
	copy(sum224, sum.left(Size224))
	return sum224
}

// Sum512_256 returns the Sum512/256 checksum of the data.
pub fn sum512_256(data []byte) []byte {
	mut d := _new(crypto.Hash.SHA512_256)
	d.write(data)
	sum := d.checksum()
	mut sum256 := [byte(0)].repeat(Size256)
	copy(sum256, sum.left(Size256))
	return sum256
}

fn block(dig mut Digest, p []byte) {
	// For now just use block_generic until we have specific
	// architecture optimized versions
	block_generic(mut dig, p)
}

pub fn (d &Digest) size() int {
	switch d.function {
	case crypto.Hash.SHA512_224:
		return Size224
	case crypto.Hash.SHA512_256:
		return Size256
	case crypto.Hash.SHA384:
		return Size384
	default:
		return Size
	}
}

pub fn (d &Digest) block_size() int { return BlockSize }

pub fn hexhash(s string) string { return sum512(s.bytes()).hex() }
pub fn hexhash_384(s string) string { return sum384(s.bytes()).hex() }
pub fn hexhash_512_224(s string) string { return sum512_224(s.bytes()).hex() }
pub fn hexhash_512_256(s string) string { return sum512_256(s.bytes()).hex() }
