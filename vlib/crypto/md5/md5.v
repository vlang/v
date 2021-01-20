// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package md5 implements the MD5 hash algorithm as defined in RFC 1321.
// MD5 is cryptographically broken and should not be used for secure
// applications.
// Based off:   https://github.com/golang/go/blob/master/src/crypto/md5
// Last commit: https://github.com/golang/go/commit/ed7f323c8f4f6bc61a75146bf34f5b8f73063a17
module md5

import encoding.binary

pub const (
	// The size of an MD5 checksum in bytes.
	size       = 16
	// The blocksize of MD5 in bytes.
	block_size = 64
)

const (
	init0 = 0x67452301
	init1 = 0xEFCDAB89
	init2 = 0x98BADCFE
	init3 = 0x10325476
)

// Digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	s   []u32
	x   []byte
	nx  int
	len u64
}

fn (mut d Digest) reset() {
	d.s = []u32{len: (4)}
	d.x = []byte{len: (block_size)}
	d.s[0] = u32(init0)
	d.s[1] = u32(init1)
	d.s[2] = u32(init2)
	d.s[3] = u32(init3)
	d.nx = 0
	d.len = 0
}

// new returns a new Digest (implementing hash.Hash) computing the MD5 checksum.
pub fn new() &Digest {
	mut d := &Digest{}
	d.reset()
	return d
}

pub fn (mut d Digest) write(p_ []byte) int {
	unsafe {
		mut p := p_
		nn := p.len
		d.len += u64(nn)
		if d.nx > 0 {
			n := copy(d.x[d.nx..], p)
			d.nx += n
			if d.nx == block_size {
				block(mut d, d.x)
				d.nx = 0
			}
			if n >= p.len {
				p = []
			} else {
				p = p[n..]
			}
		}
		if p.len >= block_size {
			n := p.len & ~(block_size - 1)
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
		return nn
	}
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

pub fn (mut d Digest) checksum() []byte {
	// Append 0x80 to the end of the message and then append zeros
	// until the length is a multiple of 56 bytes. Finally append
	// 8 bytes representing the message length in bits.
	//
	// 1 byte end marker :: 0-63 padding bytes :: 8 byte length
	// tmp := [1 + 63 + 8]byte{0x80}
	mut tmp := []byte{len: (1 + 63 + 8)}
	tmp[0] = 0x80
	pad := ((55 - d.len) % 64) // calculate number of padding bytes
	binary.little_endian_put_u64(mut tmp[1 + pad..], d.len << 3) // append length in bits
	d.write(tmp[..1 + pad + 8])
	// The previous write ensures that a whole number of
	// blocks (i.e. a multiple of 64 bytes) have been hashed.
	if d.nx != 0 {
		panic('d.nx != 0')
	}
	mut digest := []byte{len: (size)}
	binary.little_endian_put_u32(mut digest, d.s[0])
	binary.little_endian_put_u32(mut digest[4..], d.s[1])
	binary.little_endian_put_u32(mut digest[8..], d.s[2])
	binary.little_endian_put_u32(mut digest[12..], d.s[3])
	return digest
}

// sum returns the MD5 checksum of the data.
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
