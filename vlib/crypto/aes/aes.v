// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Based off:   https://github.com/golang/go/blob/master/src/crypto/aes
// Last commit: https://github.com/golang/go/commit/691a2d457ab1bf03bd46d4b69e0f93b8993c0055

module aes

import (
	crypto.internal.subtle
)

const (
	// The AES block size in bytes.
	BlockSize = 16
)

// A cipher is an instance of AES encryption using a particular key.
struct AesCipher {
	enc []u32
	dec []u32
}

// new_cipher creates and returns a new cipher.Block.
// The key argument should be the AES key,
// either 16, 24, or 32 bytes to select
// AES-128, AES-192, or AES-256.
pub fn new_cipher(key []byte) AesCipher {
	k := key.len
	switch k {
	case 16, 24, 32:
		// break
	default:
		panic('crypto.aes: invalid key size ' + k.str())
		// return error('crypto.aes: invalid key size ' + k.str())
	}
	// for now use generic version
	return new_cipher_generic(key)
}

pub fn (c &AesCipher) block_size() int { return BlockSize }

pub fn (c &AesCipher) encrypt(dst, src []byte) {
	if src.len < BlockSize {
		panic('crypto.aes: input not full block')
	}
	if dst.len < BlockSize {
		panic('crypto.aes: output not full block')
	}
	// if subtle.inexact_overlap(dst[:BlockSize], src[:BlockSize]) {
	if subtle.inexact_overlap(dst.left(BlockSize), src.left(BlockSize)) {
		panic('crypto.aes: invalid buffer overlap')
	}
	// for now use generic version
	encrypt_block_generic(c.enc, dst, src)
}

pub fn (c &AesCipher) decrypt(dst, src []byte) {
	if src.len < BlockSize {
		panic('crypto.aes: input not full block')
	}
	if dst.len < BlockSize {
		panic('crypto.aes: output not full block')
	}
	if subtle.inexact_overlap(dst.left(BlockSize), src.left(BlockSize)) {
		panic('crypto.aes: invalid buffer overlap')
	}
	// for now use generic version
	decrypt_block_generic(c.dec, dst, src)
}
