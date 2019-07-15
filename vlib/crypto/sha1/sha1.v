// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package sha1 implements the SHA-1 hash algorithm as defined in RFC 3174.
//
// SHA-1 is cryptographically broken and should not be used for secure
// applications.
module sha1

import math
import encoding.binary

// import (
// 	"crypto"
// 	"encoding/binary"
// 	"errors"
// 	"hash"
// )

// fn init() {
// 	crypto.RegisterHash(crypto.SHA1, New)
// }


const(
	// The size of a SHA-1 checksum in bytes.
	Size     = 20
	// The blocksize of SHA-1 in bytes.
	BlockSize = 64
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
	// h   [5]u32
	// x   [chunk]byte
	// h   [5]u32
	// x   [64]byte
	h   []u32
	x   []byte
	nx  int
	// len u64
	len u64
}

const (
	magic         = 'sha\x01'
	marshaledSize = magic.len + 5*4 + chunk + 8
)

fn (d mut Digest) marshal_binary() ?[]byte {
	// b := make([]byte, 0, marshaledSize)
	mut b := []byte
	b << []byte(magic)
	b = append_u32(mut b, d.h[0])
	b = append_u32(mut b, d.h[1])
	b = append_u32(mut b, d.h[2])
	b = append_u32(mut b, d.h[3])
	b = append_u32(mut b, d.h[4])
	b << d.x.left(d.nx)
	b = b.left(b.len+d.x.len-int(d.nx)) // already zero
	b = append_u64(mut b, d.len)
	return b
}

fn (d mut Digest) unmarshal_binary(b []byte) {
	// if b.len < magic.len || string(b.left(magic.len)) != magic {
	z := b.left(magic.len)
	if b.len < magic.len || tos(z.data, z.len) != magic {
		return error('crypto/sha1: invalid hash state identifier')
	}
	if b.len != marshaledSize {
		return error('crypto/sha1: invalid hash state size')
	}
	// b = b[magic.len:]
	b = b.right(magic.len)
	mut c := consume_u32(b)
	b = c.b
	d.h[0] = c.u
	c = consume_u32(b)
	b = c.b
	d.h[1] = c.u
	c = consume_u32(b)
	b = c.b
	d.h[2] = c.u
	c = consume_u32(b)
	b = c.b
	d.h[3] = c.u
	c = consume_u32(b)
	b = c.b
	d.h[4] = c.u
	// b = b[copy(d.x[:], b):]
	min := int(math.min(f64(d.x.len), f64(b.len)))
	for i:=0; i<min; i++ {
		d.x.set(i, b[i])
	}
	b = b.right(min)
	
	cc := consume_u64(b)
	b = cc.b
	d.len = cc.u
	d.nx = int(int(d.len) % chunk)
	// return nil
}

fn append_u64(b mut []byte, x u64) []byte {
	// a := [8]byte
	mut a := []byte
	// binary.big_endian_put_u64(a[:], x)
	binary.big_endian_put_u64(a, x)
	// return append(b, a[:]...)
	// b << a
	for i in a {
		b << i
	}
	return *b
}

fn append_u32(b mut []byte, x u32) []byte {
	// a := [4]byte
	a := []byte
	// binary.big_endian_put_u32(a[:], x)
	binary.big_endian_put_u32(a, x)
	// return append(b, a[:]...)
	// b << a
	for i in a {
		b << i
	}
	return *b
}

struct ConsumedU64{
	b []byte
	u u64
}

fn consume_u64(b []byte) ConsumedU64 {
	_ := b[7]
	// x := u64(b[7]) | u64(b[6])<<8 | u64(b[5])<<16 | u64(b[4])<<24 |
	// 	u64(b[3])<<32 | u64(b[2])<<40 | u64(b[1])<<48 | u64(b[0])<<56
	x := u64(b[7]) | u64(u64(b[6])<<u64(8)) | u64(u64(b[5])<<u64(16)) | u64(u64(b[4])<<u64(24)) |
		u64(u64(b[3])<<u64(32)) | u64(u64(b[2])<<u64(40)) | u64(u64(b[1])<<u64(48)) | u64(u64(b[0])<<u64(56))
	return ConsumedU64{
		// b: b[8:]
		b: b.right(8)
		u: x
	}
}

struct ConsumedU32{
	b []byte
	u u32
}

fn consume_u32(b []byte) ConsumedU32 {
	_ := b[3]
	x := u32(b[3]) | u32(u32(b[2])<<u32(8)) | u32(u32(b[1])<<u32(16)) | u32(u32(b[0])<<u32(24))
	return ConsumedU32{
		// b: b[4:]
		b: b.right(4)
		u: x
	}
}

fn (d mut Digest) reset() {
	d.h = make_arr_u32(5)
	d.h[0] = u32(init0)
	d.h[1] = u32(init1)
	d.h[2] = u32(init2)
	d.h[3] = u32(init3)
	d.h[4] = u32(init4)
	// d.h << u32(init0)
	// d.h << u32(init1)
	// d.h << u32(init2)
	// d.h << u32(init3)
	// d.h << u32(init4)
	d.nx = 0
	d.len = u64(0)
}

// New returns a new hash.Hash computing the SHA1 checksum. The Hash also
// implements encoding.BinaryMarshaler and encoding.BinaryUnmarshaler to
// marshal and unmarshal the internal state of the hash.
// fn new() hash.Hash {
pub fn new() {
	mut d := Digest{}
	d.reset()
	return d
}

fn (d &Digest) size() int { return Size }

fn (d &Digest) block_size() int { return BlockSize }

fn (d mut Digest) write(p []byte) ?int {
	nn := p.len
	d.len += u64(nn)

	if d.nx > 0 {
		println('d.nx > 0')
		// n := copy(d.x[d.nx:], p)
		n := int(math.min(f64(d.x.len), f64(p.len)))
		for i:=d.nx; i<n; i++ {
			d.x.set(i, p[i])
		}
		d.nx += n
		if d.nx == chunk {
			block(d, d.x)
			d.nx = 0
		}
		// p = p[n:]
		p = p.right(n)
		println('P:')
		for i in p {
			C.printf(i)
		}
	}
	if p.len >= chunk {
		n := p.len &~ (chunk - 1)
		block(d, p.left(n))
		p = p.right(n)
	}
	if p.len > 0 {
		println('p.len > 0: $p.len')
		// d.nx = copy(d.x[:], p)
		d.nx = int(math.min(f64(d.x.len), f64(p.len)))
		for i:=0; i<d.nx; i++ {
			d.x.set(i, p[i])
		}
	}
	return nn
}

fn (d &Digest) sum(b_in mut []byte) []byte {
	// Make a copy of d so that caller can keep writing and summing.
	// mut d0 := *d
	mut d0 := d
	hash := d0.check_sum()
	// return append(b_in, hash[:]...)
	for b in hash {
		b_in << b
	}
	return *b_in
}

// fn (d &Digest) check_sum() [Size]byte {
fn (d mut Digest) check_sum() []byte {
	// d.x = make_arr_byte(64)
	mut len := d.len
	// Padding.  Add a 1 bit and 0 bits until 56 bytes mod 64.
	// mut tmp := [64]byte
	mut tmp := make_arr_byte(64)

	tmp[0] = 0x80

	if int(len)%64 < 56 {
		// d.write(tmp[0 : 56-len%64])
		d.write(tmp.left(56-int(len)%64))
	} else {
		d.write(tmp.left(64+56-int(len)%64))
	}
	C.printf('b:\n')
    for i in d.x {
        C.printf('%02x', i)
    }

	// tmp << 
	// Length in bits.
	len <<= u64(3)
	binary.big_endian_put_u64(&tmp, len)
	d.write(tmp.left(8))

	if d.nx != 0 {
		panic('d.nx != 0')
	}

	// digest := [Size]byte
	mut digest := make_arr_byte(Size)

	// binary.big_endian_put_u32(digest[0:], d.h[0])
	// binary.big_endian_put_u32(digest[4:], d.h[1])
	// binary.big_endian_put_u32(digest[8:], d.h[2])
	// binary.big_endian_put_u32(digest[12:], d.h[3])
	// binary.big_endian_put_u32(digest[16:], d.h[4])
    for i in d.h {
        C.printf('%02x', i)
    }
	 C.printf('\n')

	// a := binary.big_endian_put_u32(digest, d.h[0])
	// C.printf('%02x\n', a)
	
	// for t := 0; t <= 16; t+=4 {
	// 	a:= digest.right(t)
	// 	for i := 0; i <= 4; t++ {
	// 	b := binary.big_endian_put_u32
	// 	digest[t] = big_endian_put_u32(d.h[t])
	// 	count++
	// }

	binary.big_endian_put_u32(digest, d.h[0])
	binary.big_endian_put_u32(digest.right(4), d.h[1])
	binary.big_endian_put_u32(digest.right(8), d.h[2])
	binary.big_endian_put_u32(digest.right(12), d.h[3])
	binary.big_endian_put_u32(digest.right(16), d.h[4])

	return digest
}

// ConstantTimeSum computes the same result of Sum() but in constant time
fn (d &Digest) constant_time_sum(b_in mut []byte) []byte {
	// mut d0 := *d
	mut d0 := *d
	hash := d0.const_sum()
	// return append(b_in, hash[:]...)
	for b in hash {
		b_in << b
	}
	return *b_in  
}

// fn (d &Digest) const_sum() [Size]byte {
fn (d mut Digest) const_sum() []byte {
	mut length := make_arr_byte(8)
	l := d.len << u64(3)
	for i := 0; i < 8; i++ {
		length[i] = byte(l >> (56 - 8*i))
	}

	nx := byte(d.nx)
	t := nx - 56                 // if nx < 56 then the MSB of t is one
	mask1b := byte(i8(t) >> i8(7)) // mask1b is 0xFF iff one block is enough

	mut separator := byte(0x80) // gets reset to 0x00 once used
	for i := byte(0); i < chunk; i++ {
		mask := byte(i8(i-nx) >> i8(7)) // 0x00 after the end of data

		// if we reached the end of the data, replace with 0x80 or 0x00
		d.x[i] = (~mask & separator) | (mask & d.x[i])

		// zero the separator once used
		separator &= mask

		if i >= 56 {
			// we might have to write the length here if all fit in one block
			d.x[i] = d.x[i] | (mask1b & length[i-56])
		}
	}

	// compress, and only keep the digest if all fit in one block
	block(d, d.x)

	// mut digest := [Size]byte
	mut digest := make_arr_byte(Size)
	// for i, s := range d.h {
	for i, s in d.h {
		digest[i*4] = mask1b & byte(s>>u32(24))
		digest[i*4+1] = mask1b & byte(s>>u32(16))
		digest[i*4+2] = mask1b & byte(s>>u32(8))
		digest[i*4+3] = mask1b & byte(s)
	}

	for i := byte(0); i < chunk; i++ {
		// second block, it's always past the end of data, might start with 0x80
		if i < 56 {
			d.x[i] = separator
			separator = 0
		} else {
			d.x[i] = length[i-56]
		}
	}

	// compress, and only keep the digest if we actually needed the second block
	block(d, d.x)

	for i, s in d.h {
		digest[i*4] = digest[i*4]| (~mask1b & byte(s>>u32(24)))
		digest[i*4+1] = digest[i*4+1] | (~mask1b & byte(s>>u32(16)))
		digest[i*4+2] = digest[i*4+2] | (~mask1b & byte(s>>u32(8)))
		digest[i*4+3] = digest[i*4+3] | (~mask1b & byte(s))
	}

	return digest
}

// Sum returns the SHA-1 checksum of the data.
// fn sum(data []byte) [Size]byte {
pub fn sum(data []byte) []byte {
	mut d := Digest{}
	d.reset()
	d.write(data)
	C.printf('%02x', d.x)
	return d.check_sum()
}


pub fn make_arr_byte(size int) []byte {
	mut b := []byte
	for i:=0; i<size; i++ {
		b << 0
	}
	return b
}
pub fn make_arr_u32(size int) []u32 {
	mut b := []u32
	for i:=0; i<size; i++ {
		b << u32(0)
	}
	return b
}