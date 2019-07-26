// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Package rc4 implements RC4 encryption, as defined in Bruce Schneier's
// Applied Cryptography.
//
// RC4 is cryptographically broken and should not be used for secure
// applications.

// Adapted from: https://github.com/golang/go/blob/master/src/crypto/rc4

module rc4

import crypto.internal.subtle

// A Cipher is an instance of RC4 using a particular key.
struct Cipher {
mut:
	s []u32
	i u8
	j u8
}

// new_cipher creates and returns a new Cipher. The key argument should be the
// RC4 key, at least 1 byte and at most 256 bytes.
pub fn new_cipher(key []byte) ?Cipher {
	if key.len < 1 || key.len > 256 {
		return error('crypto.rc4: invalid key size ' + key.len.str())
	}
	mut c := Cipher{
		s: [u32(0); 256]
	}
	for i := 0; i < 256; i++ {
		c.s[i] = u32(i)
	}
	mut j := u8(0)
	for i := 0; i < 256; i++ {
		j += u8(c.s[i]) + u8(key[i%key.len])
		tmp := c.s[i]
		c.s[i] = c.s[j]
		c.s[j] = tmp
	}
	return c
}

// reset zeros the key data and makes the Cipher unusable.
//
// Deprecated: Reset can't guarantee that the key will be entirely removed from
// the process's memory.
pub fn (c &Cipher) reset() {
	for i in c.s {
		c.s[i] = u32(0)
	}
	c.i = u8(0)
	c.j = u8(0)
}

// xor_key_stream sets dst to the result of XORing src with the key stream.
// Dst and src must overlap entirely or not at all.
pub fn (c &Cipher) xor_key_stream(dst, src []byte) {
	if src.len == 0 {
		return
	}
	if subtle.inexact_overlap(dst.left(src.len), src) {
		panic('crypto.rc4: invalid buffer overlap')
	}
	mut i := c.i
	mut j := c.j
	_ := dst[src.len-1]
	dst = dst.left(src.len) // eliminate bounds check from loop
	for k, v in src {
		i += u8(1)
		x := c.s[i]
		j += u8(x)
		y := c.s[j]
		c.s[i] = y
		c.s[j] = x
		dst[k] = v ^ byte(c.s[u8(x+y)])
	}
	c.i = i
	c.j = j
}
