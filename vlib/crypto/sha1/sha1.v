// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package sha1 implements the SHA-1 hash algorithm as defined in RFC 3174.
// SHA-1 is cryptographically broken and should not be used for secure
// applications.
// Based off:   https://github.com/golang/go/blob/master/src/crypto/sha1
// Last commit: https://github.com/golang/go/commit/3ce865d7a0b88714cc433454ae2370a105210c01
module sha1

// The size of a SHA-1 checksum in bytes.
pub const size = 20
// The blocksize of SHA-1 in bytes.
pub const block_size = 64

const chunk = 64
const init0 = u32(0x67452301)
const init1 = u32(0xEFCDAB89)
const init2 = u32(0x98BADCFE)
const init3 = u32(0x10325476)
const init4 = u32(0xC3D2E1F0)

// digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	h   []u32
	x   []u8
	nx  int
	len u64
}

// free the resources taken by the Digest `d`
@[unsafe]
pub fn (mut d Digest) free() {
	$if prealloc {
		return
	}
	unsafe {
		d.x.free()
		d.h.free()
	}
}

fn (mut d Digest) init() {
	d.x = []u8{len: chunk}
	d.h = []u32{len: (5)}
	d.reset()
}

// reset the state of the Digest `d`
pub fn (mut d Digest) reset() {
	d.h[0] = u32(init0)
	d.h[1] = u32(init1)
	d.h[2] = u32(init2)
	d.h[3] = u32(init3)
	d.h[4] = u32(init4)
	d.nx = 0
	d.len = 0
}

fn (d &Digest) clone() &Digest {
	return &Digest{
		...d
		h: d.h.clone()
		x: d.x.clone()
	}
}

// copy_from overwrites the state of `d` with the state of `src`, without allocating.
// Afterwards `d` continues from the data that was written to `src` so far.
pub fn (mut d Digest) copy_from(src &Digest) {
	copy(mut d.h, src.h)
	copy(mut d.x, src.x[..src.nx])
	d.nx = src.nx
	d.len = src.len
}

// new returns a new Digest (implementing hash.Hash) computing the SHA1 checksum.
pub fn new() &Digest {
	mut d := &Digest{}
	d.init()
	return d
}

// write writes the contents of `p_` to the internal hash representation.
@[manualfree]
pub fn (mut d Digest) write(p_ []u8) !int {
	nn := p_.len
	unsafe {
		mut p := p_
		d.len += u64(nn)
		if d.nx > 0 {
			n := copy(mut d.x[d.nx..], p)
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
			d.nx = copy(mut d.x, p)
		}
	}
	return nn
}

// sum returns a copy of the generated sum of the bytes in `b_in`.
pub fn (d &Digest) sum(b_in []u8) []u8 {
	// Make a copy of d so that caller can keep writing and summing.
	mut d0 := d.clone()
	hash := d0.checksum()
	mut b_out := b_in.clone()
	for b in hash {
		b_out << b
	}
	return b_out
}

// checksum returns the current byte checksum of the `Digest`,
fn (mut d Digest) checksum() []u8 {
	mut digest := []u8{len: size}
	d.checksum_into(mut digest)
	return digest
}

// checksum_into finalizes `d` and writes its checksum into the first `size`
// bytes of `out`, without allocating. It panics if `out` is shorter than `size`.
// Like `sum`, it consumes the state of `d`: call `reset()` or `copy_from()`
// before writing to `d` again.
@[direct_array_access]
pub fn (mut d Digest) checksum_into(mut out []u8) {
	if out.len < size {
		panic('sha1: checksum_into: `out` must be at least ${size} bytes long')
	}
	d.pad()
	for i in 0 .. size {
		out[i] = u8(d.h[i >> 2] >> (24 - 8 * (i & 3)))
	}
}

// pad writes the final padding and the message length to `d`, so that `d.h`
// holds the final hash value.
@[direct_array_access]
fn (mut d Digest) pad() {
	mut len := d.len
	// Padding.  Add a 1 bit and 0 bits until 56 bytes mod 64.
	// `tmp` is on the stack, so finalizing a digest does not allocate.
	mut tmp := [chunk]u8{}
	tmp[0] = 0x80
	// Reduce before narrowing so cumulative lengths fit on 32-bit targets.
	remainder := int(len % u64(chunk))
	n := if remainder < 56 { 56 - remainder } else { chunk + 56 - remainder }
	// vbytes wraps `tmp` without copying it; slicing a fixed array would allocate.
	d.write(unsafe { (&tmp[0]).vbytes(n) }) or { panic(err) }
	// Length in bits.
	len <<= 3
	for i in 0 .. 8 {
		tmp[i] = u8(len >> (56 - 8 * i))
	}
	d.write(unsafe { (&tmp[0]).vbytes(8) }) or { panic(err) }
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
	return size
}

// block_size returns the block size of the checksum in bytes.
pub fn (d &Digest) block_size() int {
	return block_size
}

// hexhash returns a hexadecimal SHA1 hash sum `string` of `s`.
pub fn hexhash(s string) string {
	return sum(s.bytes()).hex()
}
