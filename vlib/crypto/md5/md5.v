// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Package md5 implements the MD5 hash algorithm as defined in RFC 1321.

// MD5 is cryptographically broken and should not be used for secure
// applications.

// Adapted from: https://github.com/golang/go/blob/master/src/crypto/md5

module md5

import math
import encoding.binary

const (
	// The size of an MD5 checksum in bytes.
	Size = 16
	// The blocksize of MD5 in bytes.
	BlockSize = 64
)

const (
	Init0 = 0x67452301
	Init1 = 0xEFCDAB89
	Init2 = 0x98BADCFE
	Init3 = 0x10325476
)

// Digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	s   []u32
	x   []byte
	nx  int
	len u64
}

fn (d mut Digest) reset() {
	d.s = [u32(0); 4]
	d.x = [byte(0); BlockSize]
    d.s[0] = u32(Init0)
	d.s[1] = u32(Init1)
	d.s[2] = u32(Init2)
	d.s[3] = u32(Init3)
	d.nx = 0
	d.len = u64(0)
}

// new returns a new Digest (implementing hash.Hash) computing the MD5 checksum.
pub fn new() *Digest {
	mut d := &Digest{}
	d.reset()
	return d
}

pub fn (d mut Digest) write(p []byte) ?int {
	nn := p.len
	d.len += u64(nn)
	if d.nx > 0 {
		n := int(math.min(f64(d.x.len), f64(p.len)))
		for i:=0; i<n; i++ {
			d.x.set(i+d.nx, p[i])
		}
		d.nx += n
		if d.nx == BlockSize {
            block(d, d.x)
			d.nx = 0
		}
		if n >= p.len {
			p = []byte
		} else {
			p = p.right(n)
		}
	}
	if p.len >= BlockSize {
		n := p.len &~ (BlockSize - 1)
		block(d, p.left(n))
		if n >= p.len {
			p = []byte
		} else {
			p = p.right(n)
		}
	}
	if p.len > 0 {
		d.nx = int(math.min(f64(d.x.len), f64(p.len)))
		for i:=0; i<d.nx; i++ {
			d.x.set(i, p[i])
		}
	}
	return nn
}

pub fn (d &Digest) sum(b_in mut []byte) []byte {
	// Make a copy of d so that caller can keep writing and summing.
	mut d0 := *d
	hash := d0.checksum()
	for b in hash {
		b_in << b
	}
	return *b_in
}

pub fn (d mut Digest) checksum() []byte {
	// Append 0x80 to the end of the message and then append zeros
	// until the length is a multiple of 56 bytes. Finally append
	// 8 bytes representing the message length in bits.
	//
	// 1 byte end marker :: 0-63 padding bytes :: 8 byte length
	// tmp := [1 + 63 + 8]byte{0x80}
    mut tmp := [byte(0); 1 + 63 + 8]
	tmp[0] = 0x80
	pad := (55 - int(d.len)) % 64 // calculate number of padding bytes
	binary.little_endian_put_u64(tmp.right(1+pad), u64(d.len<<u64(3))) // append length in bits
    d.write(tmp.left(1+pad+8))

	// The previous write ensures that a whole number of
	// blocks (i.e. a multiple of 64 bytes) have been hashed.
	if d.nx != 0 {
		panic('d.nx != 0')
	}

    digest := [byte(0); Size]

	binary.little_endian_put_u32(digest, d.s[0])
	binary.little_endian_put_u32(digest.right(4), d.s[1])
	binary.little_endian_put_u32(digest.right(8), d.s[2])
	binary.little_endian_put_u32(digest.right(12), d.s[3])
	return digest
}

// sum returns the MD5 checksum of the data.
pub fn sum(data []byte) []byte {
	mut d := new()
	d.write(data)
	return d.checksum()
}

fn block(dig &Digest, p []byte) {
    // For now just use block_generic until we have specific
	// architecture optimized versions
    block_generic(dig, p)
}

pub fn (d &Digest) size() int { return Size }

pub fn (d &Digest) block_size() int { return BlockSize }
