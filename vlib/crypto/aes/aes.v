// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Based off:   https://github.com/golang/go/blob/master/src/crypto/aes
// Last commit: https://github.com/golang/go/commit/691a2d457ab1bf03bd46d4b69e0f93b8993c0055
module aes

import crypto.internal.subtle

pub const (
	// The AES block size in bytes.
	block_size = 16
)

// AesCipher represents an AES encryption using a particular key.
struct AesCipher {
mut:
	enc []u32
	dec []u32
}

// new_cipher creates and returns a new `AesCipher`.
// The key argument should be the AES key,
// either 16, 24, or 32 bytes to select
// AES-128, AES-192, or AES-256.
pub fn new_cipher(key []byte) AesCipher {
	k := key.len
	match k {
		16, 24, 32 {
			// break
		}
		else {
			panic('crypto.aes: invalid key size ' + k.str())
			// return error('crypto.aes: invalid key size ' + k.str())
		}
	}
	// for now use generic version
	return new_cipher_generic(key)
}

// block_size returns the block size of the checksum in bytes.
pub fn (c &AesCipher) block_size() int {
	return block_size
}

// encrypt encrypts the blocks in `src` to `dst`.
// Please note: `dst` and `src` are both mutable for performance reasons.
pub fn (c &AesCipher) encrypt(mut dst []byte, mut src []byte) {
	if src.len < block_size {
		panic('crypto.aes: input not full block')
	}
	if dst.len < block_size {
		panic('crypto.aes: output not full block')
	}
	// if subtle.inexact_overlap(dst[:block_size], src[:block_size]) {
	if subtle.inexact_overlap((*dst)[..block_size], (*src)[..block_size]) {
		panic('crypto.aes: invalid buffer overlap')
	}
	// for now use generic version
	encrypt_block_generic(c.enc, mut dst, src)
}

// decrypt decrypts the blocks in `src` to `dst`.
// Please note: `dst` and `src` are both mutable for performance reasons.
pub fn (c &AesCipher) decrypt(mut dst []byte, mut src []byte) {
	if src.len < block_size {
		panic('crypto.aes: input not full block')
	}
	if dst.len < block_size {
		panic('crypto.aes: output not full block')
	}
	if subtle.inexact_overlap((*dst)[..block_size], (*src)[..block_size]) {
		panic('crypto.aes: invalid buffer overlap')
	}
	// for now use generic version
	decrypt_block_generic(c.dec, mut dst, src)
}
