module pkcs12

import crypto.cipher
import crypto.subtle

const rc2_block_size = 8

const rc2_pitable = [u8(0xd9), 0x78, 0xf9, 0xc4, 0x19, 0xdd, 0xb5, 0xed, 0x28, 0xe9, 0xfd, 0x79,
	0x4a, 0xa0, 0xd8, 0x9d, 0xc6, 0x7e, 0x37, 0x83, 0x2b, 0x76, 0x53, 0x8e, 0x62, 0x4c, 0x64, 0x88,
	0x44, 0x8b, 0xfb, 0xa2, 0x17, 0x9a, 0x59, 0xf5, 0x87, 0xb3, 0x4f, 0x13, 0x61, 0x45, 0x6d, 0x8d,
	0x09, 0x81, 0x7d, 0x32, 0xbd, 0x8f, 0x40, 0xeb, 0x86, 0xb7, 0x7b, 0x0b, 0xf0, 0x95, 0x21, 0x22,
	0x5c, 0x6b, 0x4e, 0x82, 0x54, 0xd6, 0x65, 0x93, 0xce, 0x60, 0xb2, 0x1c, 0x73, 0x56, 0xc0, 0x14,
	0xa7, 0x8c, 0xf1, 0xdc, 0x12, 0x75, 0xca, 0x1f, 0x3b, 0xbe, 0xe4, 0xd1, 0x42, 0x3d, 0xd4, 0x30,
	0xa3, 0x3c, 0xb6, 0x26, 0x6f, 0xbf, 0x0e, 0xda, 0x46, 0x69, 0x07, 0x57, 0x27, 0xf2, 0x1d, 0x9b,
	0xbc, 0x94, 0x43, 0x03, 0xf8, 0x11, 0xc7, 0xf6, 0x90, 0xef, 0x3e, 0xe7, 0x06, 0xc3, 0xd5, 0x2f,
	0xc8, 0x66, 0x1e, 0xd7, 0x08, 0xe8, 0xea, 0xde, 0x80, 0x52, 0xee, 0xf7, 0x84, 0xaa, 0x72, 0xac,
	0x35, 0x4d, 0x6a, 0x2a, 0x96, 0x1a, 0xd2, 0x71, 0x5a, 0x15, 0x49, 0x74, 0x4b, 0x9f, 0xd0, 0x5e,
	0x04, 0x18, 0xa4, 0xec, 0xc2, 0xe0, 0x41, 0x6e, 0x0f, 0x51, 0xcb, 0xcc, 0x24, 0x91, 0xaf, 0x50,
	0xa1, 0xf4, 0x70, 0x39, 0x99, 0x7c, 0x3a, 0x85, 0x23, 0xb8, 0xb4, 0x7a, 0xfc, 0x02, 0x36, 0x5b,
	0x25, 0x55, 0x97, 0x31, 0x2d, 0x5d, 0xfa, 0x98, 0xe3, 0x8a, 0x92, 0xae, 0x05, 0xdf, 0x29, 0x10,
	0x67, 0x6c, 0xba, 0xc9, 0xd3, 0x00, 0xe6, 0xcf, 0xe1, 0x9e, 0xa8, 0x2c, 0x63, 0x16, 0x01, 0x3f,
	0x58, 0xe2, 0x89, 0xa9, 0x0d, 0x38, 0x34, 0x1b, 0xab, 0x33, 0xff, 0xb0, 0xbb, 0x48, 0x0c, 0x5f,
	0xb9, 0xb1, 0xcd, 0x2e, 0xc5, 0xf3, 0xdb, 0x47, 0xe5, 0xa5, 0x9c, 0x77, 0x0a, 0xa6, 0x20, 0x68,
	0xfe, 0x7f, 0xc1, 0xad]!

// Rc2Cipher is an RC2 block cipher (RFC 2268) for PKCS#12 PBE.
struct Rc2Cipher {
	block_size int = rc2_block_size
mut:
	expanded []u16
}

// new_rc2_cipher builds an RC2 cipher from raw key bytes and effective bits.
fn new_rc2_cipher(key []u8, effective_bits int) !cipher.Block {
	if key.len < 1 || key.len > 128 {
		return error('pkcs12: bad RC2 key length')
	}
	if effective_bits < 1 || effective_bits > 1024 {
		return error('pkcs12: bad RC2 effective bits')
	}
	mut c := &Rc2Cipher{
		expanded: []u16{len: 64}
	}
	c.expand(key, effective_bits)
	return c
}

// expand runs the RFC 2268 key expansion into 64 words.
fn (mut c Rc2Cipher) expand(key []u8, effective_bits int) {
	t := key.len
	t8 := (effective_bits + 7) / 8
	shift := 8 * t8 - effective_bits
	mut tm := 0xff
	if shift > 0 && shift < 8 {
		tm = 0xff >> u8(shift)
	} else if shift >= 8 {
		tm = 0
	}
	mut l := []u8{len: 128}
	for i in 0 .. 128 {
		l[i] = key[i % t]
	}
	for i in t .. 128 {
		l[i] = rc2_pitable[(int(l[i - 1]) + int(l[i - t])) & 0xff]
	}
	l[128 - t8] = rc2_pitable[int(l[128 - t8]) & tm]
	for i := 127 - t8; i >= 0; i-- {
		l[i] = rc2_pitable[int(l[i + 1]) ^ int(l[i + t8])]
	}
	for i in 0 .. 64 {
		c.expanded[i] = u16(l[2 * i]) + (u16(l[2 * i + 1]) << 8)
	}
}

fn rc2_rol(x u16, n u32) u16 {
	s := n & 15
	if s == 0 {
		return x
	}
	return u16((u32(x) << s) | (u32(x) >> (16 - s)))
}

fn rc2_ror(x u16, n u32) u16 {
	s := n & 15
	if s == 0 {
		return x
	}
	return u16((u32(x) >> s) | (u32(x) << (16 - s)))
}

// block_size returns the RC2 block size.
pub fn (c &Rc2Cipher) block_size() int {
	return rc2_block_size
}

// encrypt encrypts one 8-byte block from `src` into `dst`.
pub fn (c &Rc2Cipher) encrypt(mut dst []u8, src []u8) {
	if src.len < rc2_block_size {
		panic('pkcs12/rc2: input not full block')
	}
	if dst.len < rc2_block_size {
		panic('pkcs12/rc2: output not full block')
	}
	if subtle.inexact_overlap(dst[..rc2_block_size], src[..rc2_block_size]) {
		panic('pkcs12/rc2: invalid buffer overlap')
	}
	mut r := [4]u16{}
	for i in 0 .. 4 {
		r[i] = u16(src[2 * i]) | (u16(src[2 * i + 1]) << 8)
	}
	mut j := 0
	s := [u32(1), 2, 3, 5]
	for _ in 0 .. 5 {
		for i in 0 .. 4 {
			prev1 := r[(i + 3) & 3]
			prev2 := r[(i + 2) & 3]
			prev3 := r[(i + 1) & 3]
			r[i] = r[i] + c.expanded[j] + (prev1 & prev2) + ((~prev1) & prev3)
			j++
			r[i] = rc2_rol(r[i], s[i & 3])
		}
	}
	for i in 0 .. 4 {
		prev := r[(i + 3) & 3]
		r[i] = r[i] + c.expanded[int(prev) & 63]
	}
	for _ in 0 .. 6 {
		for i in 0 .. 4 {
			prev1 := r[(i + 3) & 3]
			prev2 := r[(i + 2) & 3]
			prev3 := r[(i + 1) & 3]
			r[i] = r[i] + c.expanded[j] + (prev1 & prev2) + ((~prev1) & prev3)
			j++
			r[i] = rc2_rol(r[i], s[i & 3])
		}
	}
	for i in 0 .. 4 {
		prev := r[(i + 3) & 3]
		r[i] = r[i] + c.expanded[int(prev) & 63]
	}
	for _ in 0 .. 5 {
		for i in 0 .. 4 {
			prev1 := r[(i + 3) & 3]
			prev2 := r[(i + 2) & 3]
			prev3 := r[(i + 1) & 3]
			r[i] = r[i] + c.expanded[j] + (prev1 & prev2) + ((~prev1) & prev3)
			j++
			r[i] = rc2_rol(r[i], s[i & 3])
		}
	}
	for i in 0 .. 4 {
		dst[2 * i] = u8(r[i] & 0xff)
		dst[2 * i + 1] = u8(r[i] >> 8)
	}
}

// decrypt decrypts one 8-byte block from `src` into `dst`.
pub fn (c &Rc2Cipher) decrypt(mut dst []u8, src []u8) {
	if src.len < rc2_block_size {
		panic('pkcs12/rc2: input not full block')
	}
	if dst.len < rc2_block_size {
		panic('pkcs12/rc2: output not full block')
	}
	if subtle.inexact_overlap(dst[..rc2_block_size], src[..rc2_block_size]) {
		panic('pkcs12/rc2: invalid buffer overlap')
	}
	mut r := [4]u16{}
	for i in 0 .. 4 {
		r[i] = u16(src[2 * i]) | (u16(src[2 * i + 1]) << 8)
	}
	mut j := 63
	s := [u32(1), 2, 3, 5]
	for _ in 0 .. 5 {
		for i := 3; i >= 0; i-- {
			r[i] = rc2_ror(r[i], s[i & 3])
			prev1 := r[(i + 3) & 3]
			prev2 := r[(i + 2) & 3]
			prev3 := r[(i + 1) & 3]
			r[i] = r[i] - c.expanded[j] - (prev1 & prev2) - ((~prev1) & prev3)
			j--
		}
	}
	for i := 3; i >= 0; i-- {
		prev := r[(i + 3) & 3]
		r[i] = r[i] - c.expanded[int(prev) & 63]
	}
	for _ in 0 .. 6 {
		for i := 3; i >= 0; i-- {
			r[i] = rc2_ror(r[i], s[i & 3])
			prev1 := r[(i + 3) & 3]
			prev2 := r[(i + 2) & 3]
			prev3 := r[(i + 1) & 3]
			r[i] = r[i] - c.expanded[j] - (prev1 & prev2) - ((~prev1) & prev3)
			j--
		}
	}
	for i := 3; i >= 0; i-- {
		prev := r[(i + 3) & 3]
		r[i] = r[i] - c.expanded[int(prev) & 63]
	}
	for _ in 0 .. 5 {
		for i := 3; i >= 0; i-- {
			r[i] = rc2_ror(r[i], s[i & 3])
			prev1 := r[(i + 3) & 3]
			prev2 := r[(i + 2) & 3]
			prev3 := r[(i + 1) & 3]
			r[i] = r[i] - c.expanded[j] - (prev1 & prev2) - ((~prev1) & prev3)
			j--
		}
	}
	for i in 0 .. 4 {
		dst[2 * i] = u8(r[i] & 0xff)
		dst[2 * i + 1] = u8(r[i] >> 8)
	}
}
