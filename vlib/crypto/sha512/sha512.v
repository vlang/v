// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package sha512 implements the SHA-384, SHA-512, SHA-512/224, and SHA-512/256
// hash algorithms as defined in FIPS 180-4.
// Based off:   https://github.com/golang/go/tree/master/src/crypto/sha512
// Last commit: https://github.com/golang/go/commit/3ce865d7a0b88714cc433454ae2370a105210c01
module sha512

import crypto
import encoding.binary

pub const (
	// size is the size, in bytes, of a SHA-512 checksum.
	size       = 64
	// size224 is the size, in bytes, of a SHA-512/224 checksum.
	size224    = 28
	// size256 is the size, in bytes, of a SHA-512/256 checksum.
	size256    = 32
	// size384 is the size, in bytes, of a SHA-384 checksum.
	size384    = 48
	// block_size is the block size, in bytes, of the SHA-512/224,
	// SHA-512/256, SHA-384 and SHA-512 hash functions.
	block_size = 128
)

const (
	chunk     = 128
	init0     = u64(0x6a09e667f3bcc908)
	init1     = u64(0xbb67ae8584caa73b)
	init2     = u64(0x3c6ef372fe94f82b)
	init3     = u64(0xa54ff53a5f1d36f1)
	init4     = u64(0x510e527fade682d1)
	init5     = u64(0x9b05688c2b3e6c1f)
	init6     = u64(0x1f83d9abfb41bd6b)
	init7     = u64(0x5be0cd19137e2179)
	init0_224 = u64(0x8c3d37c819544da2)
	init1_224 = u64(0x73e1996689dcd4d6)
	init2_224 = u64(0x1dfab7ae32ff9c82)
	init3_224 = u64(0x679dd514582f9fcf)
	init4_224 = u64(0x0f6d2b697bd44da8)
	init5_224 = u64(0x77e36f7304c48942)
	init6_224 = u64(0x3f9d85a86a1d36c8)
	init7_224 = u64(0x1112e6ad91d692a1)
	init0_256 = u64(0x22312194fc2bf72c)
	init1_256 = u64(0x9f555fa3c84c64c2)
	init2_256 = u64(0x2393b86b6f53b151)
	init3_256 = u64(0x963877195940eabd)
	init4_256 = u64(0x96283ee2a88effe3)
	init5_256 = u64(0xbe5e1e2553863992)
	init6_256 = u64(0x2b0199fc2c85b8aa)
	init7_256 = u64(0x0eb72ddc81c52ca2)
	init0_384 = u64(0xcbbb9d5dc1059ed8)
	init1_384 = u64(0x629a292a367cd507)
	init2_384 = u64(0x9159015a3070dd17)
	init3_384 = u64(0x152fecd8f70e5939)
	init4_384 = u64(0x67332667ffc00b31)
	init5_384 = u64(0x8eb44a8768581511)
	init6_384 = u64(0xdb0c2e0d64f98fa7)
	init7_384 = u64(0x47b5481dbefa4fa4)
)

// Digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	h        []u64
	x        []u8
	nx       int
	len      u64
	function crypto.Hash
}

fn (mut d Digest) reset() {
	d.h = []u64{len: (8)}
	d.x = []u8{len: sha512.chunk}
	match d.function {
		.sha384 {
			d.h[0] = sha512.init0_384
			d.h[1] = sha512.init1_384
			d.h[2] = sha512.init2_384
			d.h[3] = sha512.init3_384
			d.h[4] = sha512.init4_384
			d.h[5] = sha512.init5_384
			d.h[6] = sha512.init6_384
			d.h[7] = sha512.init7_384
		}
		.sha512_224 {
			d.h[0] = sha512.init0_224
			d.h[1] = sha512.init1_224
			d.h[2] = sha512.init2_224
			d.h[3] = sha512.init3_224
			d.h[4] = sha512.init4_224
			d.h[5] = sha512.init5_224
			d.h[6] = sha512.init6_224
			d.h[7] = sha512.init7_224
		}
		.sha512_256 {
			d.h[0] = sha512.init0_256
			d.h[1] = sha512.init1_256
			d.h[2] = sha512.init2_256
			d.h[3] = sha512.init3_256
			d.h[4] = sha512.init4_256
			d.h[5] = sha512.init5_256
			d.h[6] = sha512.init6_256
			d.h[7] = sha512.init7_256
		}
		else {
			d.h[0] = sha512.init0
			d.h[1] = sha512.init1
			d.h[2] = sha512.init2
			d.h[3] = sha512.init3
			d.h[4] = sha512.init4
			d.h[5] = sha512.init5
			d.h[6] = sha512.init6
			d.h[7] = sha512.init7
		}
	}
	d.nx = 0
	d.len = 0
}

// internal
fn new_digest(hash crypto.Hash) &Digest {
	mut d := &Digest{
		function: hash
	}
	d.reset()
	return d
}

// new returns a new Digest (implementing hash.Hash) computing the SHA-512 checksum.
pub fn new() &Digest {
	return new_digest(.sha512)
}

// new512_224 returns a new Digest (implementing hash.Hash) computing the SHA-512/224 checksum.
fn new512_224() &Digest {
	return new_digest(.sha512_224)
}

// new512_256 returns a new Digest (implementing hash.Hash) computing the SHA-512/256 checksum.
fn new512_256() &Digest {
	return new_digest(.sha512_256)
}

// new384 returns a new Digest (implementing hash.Hash) computing the SHA-384 checksum.
fn new384() &Digest {
	return new_digest(.sha384)
}

// write writes the contents of `p_` to the internal hash representation.
pub fn (mut d Digest) write(p_ []u8) !int {
	unsafe {
		mut p := p_
		nn := p.len
		d.len += u64(nn)
		if d.nx > 0 {
			n := copy(mut d.x[d.nx..], p)
			d.nx += n
			if d.nx == sha512.chunk {
				block(mut d, d.x)
				d.nx = 0
			}
			if n >= p.len {
				p = []
			} else {
				p = p[n..]
			}
		}
		if p.len >= sha512.chunk {
			n := p.len & ~(sha512.chunk - 1)
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
		return nn
	}
}

// sum returns the SHA512 or SHA384 checksum of digest with the data bytes in `b_in`
pub fn (d &Digest) sum(b_in []u8) []u8 {
	// Make a copy of d so that caller can keep writing and summing.
	mut d0 := *d
	hash := d0.checksum()
	mut b_out := b_in.clone()
	match d0.function {
		.sha384 {
			for b in hash[..sha512.size384] {
				b_out << b
			}
		}
		.sha512_224 {
			for b in hash[..sha512.size224] {
				b_out << b
			}
		}
		.sha512_256 {
			for b in hash[..sha512.size256] {
				b_out << b
			}
		}
		else {
			for b in hash {
				b_out << b
			}
		}
	}
	return b_out
}

// checksum returns the current byte checksum of the Digest.
pub fn (mut d Digest) checksum() []u8 {
	// Padding. Add a 1 bit and 0 bits until 112 bytes mod 128.
	mut len := d.len
	mut tmp := []u8{len: (128)}
	tmp[0] = 0x80
	if int(len) % 128 < 112 {
		d.write(tmp[..112 - int(len) % 128]) or { panic(err) }
	} else {
		d.write(tmp[..128 + 112 - int(len) % 128]) or { panic(err) }
	}
	// Length in bits.
	len <<= u64(3)
	binary.big_endian_put_u64(mut tmp, u64(0)) // upper 64 bits are always zero, because len variable has type u64
	binary.big_endian_put_u64(mut tmp[8..], len)
	d.write(tmp[..16]) or { panic(err) }
	if d.nx != 0 {
		panic('d.nx != 0')
	}
	mut digest := []u8{len: sha512.size}
	binary.big_endian_put_u64(mut digest, d.h[0])
	binary.big_endian_put_u64(mut digest[8..], d.h[1])
	binary.big_endian_put_u64(mut digest[16..], d.h[2])
	binary.big_endian_put_u64(mut digest[24..], d.h[3])
	binary.big_endian_put_u64(mut digest[32..], d.h[4])
	binary.big_endian_put_u64(mut digest[40..], d.h[5])
	if d.function != .sha384 {
		binary.big_endian_put_u64(mut digest[48..], d.h[6])
		binary.big_endian_put_u64(mut digest[56..], d.h[7])
	}
	return digest
}

// sum512 returns the SHA512 checksum of the data.
pub fn sum512(data []u8) []u8 {
	mut d := new_digest(.sha512)
	d.write(data) or { panic(err) }
	return d.checksum()
}

// sum384 returns the SHA384 checksum of the data.
pub fn sum384(data []u8) []u8 {
	mut d := new_digest(.sha384)
	d.write(data) or { panic(err) }
	sum := d.checksum()
	mut sum384 := []u8{len: sha512.size384}
	copy(mut sum384, sum[..sha512.size384])
	return sum384
}

// sum512_224 returns the Sum512/224 checksum of the data.
pub fn sum512_224(data []u8) []u8 {
	mut d := new_digest(.sha512_224)
	d.write(data) or { panic(err) }
	sum := d.checksum()
	mut sum224 := []u8{len: sha512.size224}
	copy(mut sum224, sum[..sha512.size224])
	return sum224
}

// sum512_256 returns the Sum512/256 checksum of the data.
pub fn sum512_256(data []u8) []u8 {
	mut d := new_digest(.sha512_256)
	d.write(data) or { panic(err) }
	sum := d.checksum()
	mut sum256 := []u8{len: sha512.size256}
	copy(mut sum256, sum[..sha512.size256])
	return sum256
}

fn block(mut dig Digest, p []u8) {
	// For now just use block_generic until we have specific
	// architecture optimized versions
	block_generic(mut dig, p)
}

// size returns the size of the checksum in bytes.
pub fn (d &Digest) size() int {
	match d.function {
		.sha512_224 { return sha512.size224 }
		.sha512_256 { return sha512.size256 }
		.sha384 { return sha512.size384 }
		else { return sha512.size }
	}
}

// block_size returns the block size of the checksum in bytes.
pub fn (d &Digest) block_size() int {
	return sha512.block_size
}

// hexhash returns a hexadecimal SHA512 hash sum `string` of `s`.
pub fn hexhash(s string) string {
	return sum512(s.bytes()).hex()
}

// hexhash_384 returns a hexadecimal SHA384 hash sum `string` of `s`.
pub fn hexhash_384(s string) string {
	return sum384(s.bytes()).hex()
}

// hexhash_512_224 returns a hexadecimal SHA512/224 hash sum `string` of `s`.
pub fn hexhash_512_224(s string) string {
	return sum512_224(s.bytes()).hex()
}

// hexhash_512_256 returns a hexadecimal 512/256 hash sum `string` of `s`.
pub fn hexhash_512_256(s string) string {
	return sum512_256(s.bytes()).hex()
}
