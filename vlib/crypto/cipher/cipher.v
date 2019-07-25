// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package cipher implements standard block cipher modes that can be wrapped
// around low-level block cipher implementations.
// See https://csrc.nist.gov/groups/ST/toolkit/BCM/current_modes.html
// and NIST Special Publication 800-38A.
module cipher

// A Block represents an implementation of block cipher
// using a given key. It provides the capability to encrypt
// or decrypt individual blocks. The mode implementations
// extend that capability to streams of blocks.
interface Blocker {
	// block_size returns the cipher's block size.
	block_size() int

	// encrypt encrypts the first block in src into dst.
	// Dst and src must overlap entirely or not at all.
	encrypt(dst, src []byte)

	// decrypt decrypts the first block in src into dst.
	// Dst and src must overlap entirely or not at all.
	decrypt(dst, src []byte)
}

// A Stream represents a stream cipher.
interface Streamer {
	// xor_key_stream XORs each byte in the given slice with a byte from the
	// cipher's key stream. Dst and src must overlap entirely or not at all.
	//
	// If len(dst) < len(src), XORKeyStream should panic. It is acceptable
	// to pass a dst bigger than src, and in that case, XORKeyStream will
	// only update dst[:len(src)] and will not touch the rest of dst.
	//
	// Multiple calls to XORKeyStream behave as if the concatenation of
	// the src buffers was passed in a single run. That is, Stream
	// maintains state and does not reset at each XORKeyStream call.
	xor_key_stream(dst, src []byte)
}

// A BlockMode represents a block cipher running in a block-based mode (CBC,
// ECB etc).
interface BlockModer {
	// BlockSize returns the mode's block size.
	block_size() int

	// crypt_blocks encrypts or decrypts a number of blocks. The length of
	// src must be a multiple of the block size. Dst and src must overlap
	// entirely or not at all.
	//
	// If len(dst) < len(src), CryptBlocks should panic. It is acceptable
	// to pass a dst bigger than src, and in that case, CryptBlocks will
	// only update dst[:len(src)] and will not touch the rest of dst.
	//
	// Multiple calls to CryptBlocks behave as if the concatenation of
	// the src buffers was passed in a single run. That is, BlockMode
	// maintains state and does not reset at each CryptBlocks call.
	crypt_blocks(dst, src []byte)
}

// Utility routines

fn dup(p []byte) []byte {
	// q := make([]byte, len(p))
	mut q := [byte(0); p.len]
	// copy(q, p)
	q = p
	return q
}