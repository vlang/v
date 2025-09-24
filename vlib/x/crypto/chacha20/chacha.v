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

// Configuration options
@[params]
pub struct Options {
pub mut:
	// currently, used for XChaCha20 construct
	use_64bit_counter bool
}

// encrypt encrypts plaintext bytes with ChaCha20 cipher instance with provided key and nonce.
// It was a thin wrapper around two supported nonce size, ChaCha20 with 96 bits
// and XChaCha20 with 192 bits nonce. Internally, encrypt start with 0's counter value.
// If you want more control, use Cipher instance and setup the counter by your self.
pub fn encrypt(key []u8, nonce []u8, plaintext []u8, opt Options) ![]u8 {
	mut stream := new_stream_with_options(key, nonce, opt)!
	mut dst := []u8{len: plaintext.len}
	stream.keystream_full(mut dst, plaintext)!
	unsafe { stream.reset() }
	return dst
}

// decrypt does reverse of encrypt operation by decrypting ciphertext with ChaCha20 cipher
// instance with provided key and nonce.
pub fn decrypt(key []u8, nonce []u8, ciphertext []u8, opt Options) ![]u8 {
	mut stream := new_stream_with_options(key, nonce)!
	mut dst := []u8{len: ciphertext.len}
	stream.keystream_full(mut dst, ciphertext)!
	unsafe { stream.reset() }
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
pub fn new_cipher(key []u8, nonce []u8, opt Options) !&Cipher {
	stream := new_stream_with_options(key, nonce, opt)!
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
	dst = unsafe { dst[..src.len] }
	if subtle.inexact_overlap(dst, src) {
		panic('chacha20: invalid buffer overlap')
	}
	// index of position within src bytes
	mut idx := 0

	// First, try to drain any remaining key stream from internal buffer
	if c.length != 0 {
		// remaining keystream on internal buffer
		mut kstream := c.block[block_size - c.length..]
		if src.len < kstream.len {
			kstream = unsafe { kstream[..src.len] }
		}
		// xors every bytes in src with bytes from key stream and stored into dst
		for i, b in kstream {
			dst[idx + i] = src[idx + i] ^ b
		}
		// updates position and internal buffer length.
		// when c.length reaches the block_size, we reset it for future use.
		c.length -= kstream.len
		idx += kstream.len
		if c.length == block_size {
			unsafe { c.block.reset() }
			c.length = 0
		}
	}
	// process for remaining unprocessed src bytes
	mut remains := unsafe { src[idx..] }
	nr_blocks := remains.len / block_size

	// process for full block_size-d message
	for i := 0; i < nr_blocks; i++ {
		// for every block_sized message, we generates 64-bytes block key stream
		// and then xor-ing this block with generated key stream
		block := unsafe { remains[i * block_size..(i + 1) * block_size] }
		ks := c.keystream() or { panic(err) }
		for j, b in ks {
			dst[idx + j] = block[j] ^ b
		}
		// updates position
		idx += block_size
	}

	// process for remaining partial block
	if remains.len % block_size != 0 {
		last_block := unsafe { remains[nr_blocks * block_size..] }
		// generates one 64-bytes keystream block
		c.block = c.keystream() or { panic(err) }
		for i, b in last_block {
			dst[idx + i] = b ^ c.block[i]
		}
		c.length = block_size - last_block.len
		idx += last_block.len
	}
}

// encrypt encrypts src and stores into dst buffer. It works like `xor_key_stream` except
// its ignore key streaming process by ignoring remaining key stream in the internal buffer,
// so, its works in one shot of fashion.
// Its added to allow `chacha20poly1305` modules to work without key stream fashion.
// TODO: integrates it with the rest
@[direct_array_access]
pub fn (mut c Cipher) encrypt(mut dst []u8, src []u8) ! {
	if src.len == 0 {
		return
	}
	if dst.len < src.len {
		return error('chacha20: dst buffer is to small')
	}
	if subtle.inexact_overlap(dst, src) {
		return error('chacha20: invalid buffer overlap')
	}

	c.Stream.keystream_full(mut dst, src)!
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

// reset quickly sets all Cipher's fields to default value.
// This method will be deprecated.
@[deprecated_after: '2025-11-30']
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

// counter returns a current underlying counter value, as u64.
pub fn (c Cipher) counter() u64 {
	return c.Stream.ctr()
}

// rekey resets internal Cipher's state and reinitializes state with the provided key and nonce
pub fn (mut c Cipher) rekey(key []u8, nonce []u8) ! {
	unsafe { c.reset() }
	// we use c.Stream.mode info to get 64-bit counter capability
	w64 := if c.mode == .original { true } else { false }
	opt := Options{
		use_64bit_counter: w64
	}
	stream := new_stream_with_options(key, nonce, opt)!
	c.Stream = stream
}
