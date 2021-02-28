// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package sha256 implements the SHA224 and SHA256 hash algorithms as defined
// in FIPS 180-4.
// Based off:   https://github.com/golang/go/tree/master/src/crypto/sha256
// Last commit: https://github.com/golang/go/commit/3ce865d7a0b88714cc433454ae2370a105210c01
module sha256

import encoding.binary

pub const (
	// The size of a SHA256 checksum in bytes.
	size       = 32
	// The size of a SHA224 checksum in bytes.
	size224    = 28
	// The blocksize of SHA256 and SHA224 in bytes.
	block_size = 64
)

const (
	chunk     = 64
	init0     = 0x6A09E667
	init1     = 0xBB67AE85
	init2     = 0x3C6EF372
	init3     = 0xA54FF53A
	init4     = 0x510E527F
	init5     = 0x9B05688C
	init6     = 0x1F83D9AB
	init7     = 0x5BE0CD19
	init0_224 = 0xC1059ED8
	init1_224 = 0x367CD507
	init2_224 = 0x3070DD17
	init3_224 = 0xF70E5939
	init4_224 = 0xFFC00B31
	init5_224 = 0x68581511
	init6_224 = 0x64F98FA7
	init7_224 = 0xBEFA4FA4
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

fn (mut d Digest) reset() {
	d.h = []u32{len: (8)}
	d.x = []byte{len: (chunk)}
	if !d.is224 {
		d.h[0] = u32(init0)
		d.h[1] = u32(init1)
		d.h[2] = u32(init2)
		d.h[3] = u32(init3)
		d.h[4] = u32(init4)
		d.h[5] = u32(init5)
		d.h[6] = u32(init6)
		d.h[7] = u32(init7)
	} else {
		d.h[0] = u32(init0_224)
		d.h[1] = u32(init1_224)
		d.h[2] = u32(init2_224)
		d.h[3] = u32(init3_224)
		d.h[4] = u32(init4_224)
		d.h[5] = u32(init5_224)
		d.h[6] = u32(init6_224)
		d.h[7] = u32(init7_224)
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

// write writes the contents of `p_` to the internal hash representation.
fn (mut d Digest) write(p_ []byte) ?int {
	unsafe {
		mut p := p_
		nn := p.len
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
		return nn
	}
}

fn (d &Digest) sum(b_in []byte) []byte {
	// Make a copy of d so that caller can keep writing and summing.
	mut d0 := *d
	hash := d0.checksum()
	mut b_out := b_in.clone()
	if d0.is224 {
		for b in hash[..size224] {
			b_out << b
		}
	} else {
		for b in hash {
			b_out << b
		}
	}
	return b_out
}

fn (mut d Digest) checksum() []byte {
	mut len := d.len
	// Padding. Add a 1 bit and 0 bits until 56 bytes mod 64.
	mut tmp := []byte{len: (64)}
	tmp[0] = 0x80
	if int(len) % 64 < 56 {
		d.write(tmp[..56 - int(len) % 64]) or { panic(err) }
	} else {
		d.write(tmp[..64 + 56 - int(len) % 64]) or { panic(err) }
	}
	// Length in bits.
	len <<= u64(3)
	binary.big_endian_put_u64(mut tmp, len)
	d.write(tmp[..8]) or { panic(err) }
	if d.nx != 0 {
		panic('d.nx != 0')
	}
	mut digest := []byte{len: (size)}
	binary.big_endian_put_u32(mut digest, d.h[0])
	binary.big_endian_put_u32(mut digest[4..], d.h[1])
	binary.big_endian_put_u32(mut digest[8..], d.h[2])
	binary.big_endian_put_u32(mut digest[12..], d.h[3])
	binary.big_endian_put_u32(mut digest[16..], d.h[4])
	binary.big_endian_put_u32(mut digest[20..], d.h[5])
	binary.big_endian_put_u32(mut digest[24..], d.h[6])
	if !d.is224 {
		binary.big_endian_put_u32(mut digest[28..], d.h[7])
	}
	return digest
}

// sum returns the SHA256 checksum of the bytes in `data`.
// Example: assert sha256.sum('V'.bytes()).len > 0 == true
pub fn sum(data []byte) []byte {
	return sum256(data)
}

// sum256 returns the SHA256 checksum of the data.
pub fn sum256(data []byte) []byte {
	mut d := new()
	d.write(data) or { panic(err) }
	return d.checksum()
}

// sum224 returns the SHA224 checksum of the data.
pub fn sum224(data []byte) []byte {
	mut d := new224()
	d.write(data) or { panic(err) }
	sum := d.checksum()
	sum224 := []byte{len: (size224)}
	copy(sum224, sum[..size224])
	return sum224
}

fn block(mut dig Digest, p []byte) {
	// For now just use block_generic until we have specific
	// architecture optimized versions
	block_generic(mut dig, p)
}

// size returns the size of the checksum in bytes.
pub fn (d &Digest) size() int {
	if !d.is224 {
		return size
	}
	return size224
}

// block_size returns the block size of the checksum in bytes.
pub fn (d &Digest) block_size() int {
	return block_size
}

// hexhash returns a hexadecimal SHA256 hash sum `string` of `s`.
// Example: assert sha256.hexhash('V') == 'de5a6f78116eca62d7fc5ce159d23ae6b889b365a1739ad2cf36f925a140d0cc'
pub fn hexhash(s string) string {
	return sum256(s.bytes()).hex()
}

// hexhash_224 returns a hexadecimal SHA224 hash sum `string` of `s`.
pub fn hexhash_224(s string) string {
	return sum224(s.bytes()).hex()
}
