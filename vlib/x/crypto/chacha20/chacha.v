// Copyright (c) 2024 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Chacha20 symmetric key stream cipher encryption based on RFC 8439
module chacha20

import crypto.internal.subtle

// The size of ChaCha20 key, ie 256 bits size, in bytes
pub const key_size = 32
// The size of standard IETF ChaCha20 nonce, ie 96 bits size, in bytes
pub const nonce_size = 12
// The size of extended variant of standard ChaCha20 (XChaCha20) nonce, 192 bits
pub const x_nonce_size = 24
// The size of original ChaCha20 nonce, 64 bits
pub const orig_nonce_size = 8
// internal block size ChaCha20 operates on, in bytes
const block_size = 64

// four constants of ChaCha20 state.
const cc0 = u32(0x61707865) // expa
const cc1 = u32(0x3320646e) // nd 3
const cc2 = u32(0x79622d32) // 2-by
const cc3 = u32(0x6b206574) // te k

// CipherMode was enumeration of ChaCha20 supported variant.
enum CipherMode {
	// The standard IETF ChaCha20 (and XChaCha20), with 32-bit internal counter.
	standard
	// The original ChaCha20 with 64-bit internal counter.
	original
}

// encrypt encrypts plaintext bytes with ChaCha20 cipher instance with provided key and nonce.
// It was a thin wrapper around two supported nonce size, ChaCha20 with 96 bits
// and XChaCha20 with 192 bits nonce. Internally, encrypt start with 0's counter value.
// If you want more control, use Cipher instance and setup the counter by your self.
pub fn encrypt(key []u8, nonce []u8, plaintext []u8) ![]u8 {
	mut stream := new_stream(key, nonce)!
	mut dst := []u8{len: plaintext.len}
	stream.keystream_full(mut dst, plaintext)
	return dst
}

// decrypt does reverse of encrypt operation by decrypting ciphertext with ChaCha20 cipher
// instance with provided key and nonce.
pub fn decrypt(key []u8, nonce []u8, ciphertext []u8) ![]u8 {
	mut stream := new_stream(key, nonce)!
	mut dst := []u8{len: ciphertext.len}
	stream.keystream_full(mut dst, ciphertext)
	return dst
}

// Cipher represents ChaCha20 stream cipher instances.
@[noinit]
pub struct Cipher {
	Stream
mut:
	// internal buffer for storing key stream results
	block []u8 = []u8{len: block_size}
	// The last length of leftover unprocessed keystream from internal buffer
	length int
}

// new_cipher creates a new ChaCha20 stream cipher with the given 32 bytes key
// and bytes of nonce with supported size, ie, 8, 12 or 24 bytes nonce.
// Standard IETF variant use 12 bytes nonce's, if you want create original ChaCha20 cipher
// with support for 64-bit counter, use 8 bytes length nonce's instead
// If 24 bytes of nonce was provided, the XChaCha20 construction will be used.
// It returns new ChaCha20 cipher instance or an error if key or nonce have any other length.
pub fn new_cipher(key []u8, nonce []u8) !&Cipher {
	stream := new_stream(key, nonce)!
	return &Cipher{
		Stream: stream
	}
}

// xor_key_stream xors each byte in the given slice in the src with a byte from the
// cipher's key stream. It fulfills `cipher.Stream` interface. It encrypts the plaintext message
// in src and stores the ciphertext result in dst in a key stream fashion.
// You must never use the same (key, nonce) pair more than once for encryption.
// This would void any confidentiality guarantees for the messages encrypted with the same nonce and key.
@[direct_array_access]
pub fn (mut c Cipher) xor_key_stream(mut dst []u8, src []u8) {
	if src.len == 0 {
		return
	}
	if dst.len < src.len {
		panic('chacha20/chacha: dst buffer is to small')
	}

	mut idx := 0
	mut src_len := src.len
	// check for counter overflow
	num_blocks := (u64(src_len) + block_size - 1) / block_size
	if c.Stream.check_ctr(num_blocks) {
		panic('chacha20: internal counter overflow')
	}

	dst = unsafe { dst[..src_len] }

	if subtle.inexact_overlap(dst, src) {
		panic('chacha20: invalid buffer overlap')
	}

	// We adapt and ports the go version here
	// First, drain any remaining key stream
	if c.length != 0 {
		// remaining keystream on internal buffer
		mut kstream := c.block[block_size - c.length..]
		if src_len < kstream.len {
			kstream = unsafe { kstream[..src_len] }
		}
		for i, b in kstream {
			dst[idx + i] = src[idx + i] ^ b
		}
		// updates the idx for dst and src
		c.length -= kstream.len
		idx += kstream.len
		src_len -= kstream.len
	}

	// take the most full bytes of multiples block_size from the src,
	// build the keystream from the cipher's state and stores the result
	// into dst
	full := src_len - src_len % block_size
	if full > 0 {
		src_block := unsafe { src[idx..idx + full] }
		c.Stream.keystream_with_blocksize(mut dst[idx..idx + full], src_block)
	}
	idx += full
	src_len -= full

	// If we have a partial block, pad it for chacha20_block_generic, and
	// keep the leftover keystream for the next invocation.
	if src_len > 0 {
		// Make sure, internal buffer cleared or the old garbaged data from previous call still there
		// See the issue at https://github.com/vlang/v/issues/24043
		unsafe { c.block.reset() } //  = []u8{len: block_size}
		// copy the last src block to internal buffer, and performs
		// chacha20_block_generic on this buffer, and stores into remaining dst
		_ := copy(mut c.block, src[idx..])
		c.Stream.keystream_with_blocksize(mut c.block, c.block)
		n := copy(mut dst[idx..], c.block)
		// the length of remaining bytes of unprocessed keystream
		c.length = block_size - n
	}
}

// encrypt encrypts src and stores into dst buffer. It works like `xor_key_stream` except
// its ignore key streaming process by ignoring remaining key stream in the internal buffer,
// so, its works in one shot of fashion.
// Its added to allow `chacha20poly1305` modules to work without key stream fashion.
// TODO: integrates it with the rest
@[direct_array_access]
pub fn (mut c Cipher) encrypt(mut dst []u8, src []u8) {
	if src.len == 0 {
		return
	}
	if dst.len < src.len {
		panic('chacha20: dst buffer is to small')
	}
	if subtle.inexact_overlap(dst, src) {
		panic('chacha20: invalid buffer overlap')
	}

	c.Stream.keystream_full(mut dst, src)
}

// free the resources taken by the Cipher `c`. Dont use cipher after .free call
@[unsafe]
pub fn (mut c Cipher) free() {
	$if prealloc {
		return
	}
	unsafe {
		c.block.free()
	}
}

// reset quickly sets all Cipher's fields to default value
@[unsafe]
pub fn (mut c Cipher) reset() {
	c.Stream.reset()
	unsafe {
		c.block.reset()
	}
	c.length = 0
}

// set_counter sets Cipher's counter
pub fn (mut c Cipher) set_counter(ctr u64) {
	c.Stream.set_ctr(ctr)
}

// rekey resets internal Cipher's state and reinitializes state with the provided key and nonce
pub fn (mut c Cipher) rekey(key []u8, nonce []u8) ! {
	unsafe { c.reset() }
	stream := new_stream(key, nonce)!
	c.Stream = stream
}

// Helpers
//

// derive_xchacha20_key_nonce derives a new key and nonces for extended
// variant of Standard IETF ChaCha20 variant. Its separated for simplify the access.
@[direct_array_access; inline]
fn derive_xchacha20_key_nonce(key []u8, nonce []u8) !([]u8, []u8) {
	// Its only for x_nonce_size
	if nonce.len != x_nonce_size {
		return error('Bad nonce size for derive_xchacha20_key_nonce')
	}
	// derives a new key based on xchacha20 construction
	// first 16 bytes of nonce used to derive the key
	new_key := xchacha20(key, nonce[0..16])!
	mut new_nonce := []u8{len: nonce_size}
	// and the last of 8 bytes of nonce copied into new_nonce to build
	// nonce_size length of new_nonce
	_ := copy(mut new_nonce[4..12], nonce[16..24])

	return new_key, new_nonce
}
