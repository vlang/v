// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.cipher
import crypto.rc4

struct StreamCipher {
	cipher cipher.Stream
}

fn test_cfb_stream_cipher() ! {
	key := 'tthisisourrc4key'.bytes()
	c := rc4.new_cipher(key)!

	_ := StreamCipher{
		cipher: c
	}
}

fn test_crypto_rc4() {
	key := 'tthisisourrc4key'.bytes()

	mut c := rc4.new_cipher(key) or {
		println(err)
		return
	}

	mut src := 'toencrypt'.bytes()

	// src & dst same, encrypt in place
	c.xor_key_stream(mut src, src) // encrypt data

	c.reset()

	assert src.hex() == '189a39a91aea8afa65'
}

fn test_crypto_rc4_reset_zeroes_sbox() ! {
	key := 'Key'.bytes()
	plain := 'AAAAAAAAAAAAAAAA'.bytes()

	mut c := rc4.new_cipher(key)!
	mut first := []u8{len: plain.len}
	c.xor_key_stream(mut first, plain)

	// a second pass without a reset continues the keystream
	mut continued := []u8{len: plain.len}
	c.xor_key_stream(mut continued, plain)
	assert continued != first

	c.reset()

	// after a reset the s box is zeroed and i/j restart, so the cipher
	// is degenerate: this matches Go's crypto/rc4 Reset, which is
	// clear(c.s[:]) plus zeroed i and j
	mut after_reset := []u8{len: plain.len}
	c.xor_key_stream(mut after_reset, plain)
	assert after_reset != first
	assert after_reset != continued

	// with an all-zero s box the keystream is all zero, so the output is
	// the plaintext unchanged
	assert after_reset == plain
}
