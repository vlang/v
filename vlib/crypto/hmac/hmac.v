// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/*
Package hmac implements the Keyed-Hash Message Authentication Code (HMAC) as
defined in U.S. Federal Information Processing Standards Publication 198.
An HMAC is a cryptographic hash that uses a key to sign a message.
The receiver verifies the hash by recomputing it using the same key.
Receivers should be careful to use Equal to compare MACs in order to avoid
timing side-channels:
	// ValidMAC reports whether messageMAC is a valid HMAC tag for message.
	func ValidMAC(message, messageMAC, key []byte) bool {
		mac := hmac.New(sha256.New, key)
		mac.write(message)
		expectedMAC := mac.sum(nil)
		return hmac.Equal(messageMAC, expectedMAC)
	}
*/
module hmac

import (
	crypto.subtle
	hash
)

// FIPS 198-1:
// https://csrc.nist.gov/publications/fips/fips198-1/FIPS-198-1_final.pdf

// key is zero padded to the block size of the hash function
// ipad = 0x36 byte repeated for key length
// opad = 0x5c byte repeated for key length
// hmac = H([key ^ opad] H([key ^ ipad] text))

struct HMAC {
	size      int
	blocksize int
	opad      []byte
    ipad      []byte
	outer     hash.Hasher
    inner     hash.Hasher
}

fn (h &HMAC) sum(b_in []byte) []byte {
	orig_len := b_in.len
	b_in = h.inner.sum(b_in)
	h.outer.reset()
	h.outer.write(h.opad)
	h.outer.write(b_in.right(orig_len))
	return h.outer.sum(b_in.left(orig_len))
}

fn (h &HMAC) write(p []byte) int? {
	return h.inner.write(p)
}

fn (h &HMAC) size() int { return h.size }

fn (h &HMAC) block_size() int { return h.blocksize }

fn (h &HMAC) reset() {
	h.inner.reset()
	h.inner.write(h.ipad)
}

// new returns a new HMAC hash using the given hash.Hash type and key.
fn new(h fn() hash.Hasher, key []byte) hash.Hash {
	hm := new(hmac)
	hm.outer = h()
	hm.inner = h()
	hm.size = hm.inner.Size()
	hm.blocksize = hm.inner.BlockSize()
	hm.ipad = make([]byte, hm.blocksize)
	hm.opad = make([]byte, hm.blocksize)
	if len(key) > hm.blocksize {
		// If key is too big, hash it.
		hm.outer.write(key)
		key = hm.outer.sum(nil)
	}
	copy(hm.ipad, key)
	copy(hm.opad, key)
	for i := range hm.ipad {
		hm.ipad[i] ^= 0x36
	}
	for i := range hm.opad {
		hm.opad[i] ^= 0x5c
	}
	hm.inner.write(hm.ipad)
	return hm
}

// equal compares two MACs for equality without leaking timing information.
fn equal(mac1, mac2 []byte) bool {
	// We don't have to be constant time if the lengths of the MACs are
	// different as that suggests that a completely different hash function
	// was used.
	return subtle.ConstantTimeCompare(mac1, mac2) == 1
}