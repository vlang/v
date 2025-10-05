// Copyright (c) 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// ChaCha20-Poly1305-PSIV test.
// The test was adapted from Rust reference implementation of ChaCha20-Poly1305-PSIV
module chacha20poly1305

import rand

// test for internal encryption routine
fn test_psiv_insternal_encryption_of_encrypted_text_is_plaintext() ! {
	for i := 0; i < 1024; i++ {
		input := rand.bytes(i)!
		key := rand.bytes(36)!
		tag := rand.bytes(16)!
		nonce := rand.bytes(12)!

		out := psiv_encrypt_internal(input, key, tag, nonce)!

		// encrypting this output with the same params was result in original input
		awal := psiv_encrypt_internal(out, key, tag, nonce)!
		assert awal == input
	}
}

// test for AEAD of ChaCha20Poly1305-PSIV encrypt and decrypt
fn test_psiv_aead_encryption_of_encrypted_text_is_plaintext() ! {
	for i := 0; i < 1024; i++ {
		input := rand.bytes(i)!
		key := rand.bytes(32)!
		nonce := rand.bytes(12)!
		ad := rand.bytes(i)!

		// encrypt message input
		out := psiv_encrypt(input, key, nonce, ad)!
		// decrypting this output with the same params was resulting an original input
		awal := psiv_decrypt(out, key, nonce, ad)!
		assert awal == input

		// test with object-based construct
		mut c := new_psiv(key)!
		out1 := c.encrypt(input, nonce, ad)!
		back1 := c.decrypt(out1, nonce, ad)!
		assert back1 == input
		unsafe { c.free() }
	}
}

fn test_for_wrong_tag() ! {
	for i := 0; i < 1024; i++ {
		input := rand.bytes(i)!
		key := rand.bytes(32)!
		nonce := rand.bytes(12)!
		ad := rand.bytes(i)!

		mut out := psiv_encrypt(input, key, nonce, ad)!
		out[out.len - tag_size] ^= 1

		// decrypting would fail
		_ := psiv_decrypt(out, key, nonce, ad) or {
			assert err == error('unmatching tag')
			continue
		}
	}
}

fn test_chacha20_core() ! {
	// null input
	s := [16]u32{}
	x0 := chacha20_core(s)
	assert x0 == s

	// u32 input
	u32input := [u32(0x61707865), 0x3320646e, 0x79622d32, 0x6b206574, 0x03020100, 0x07060504,
		0x0b0a0908, 0x0f0e0d0c, 0x13121110, 0x17161514, 0x1b1a1918, 0x1f1e1d1c, 0x00000001,
		0x09000000, 0x4a000000, 0x00000000]!
	// expected output
	exp_x1 := [u32(0xe4e7f110), 0x15593bd1, 0x1fdd0f50, 0xc47120a3, 0xc7f4d1c7, 0x0368c033,
		0x9aaa2204, 0x4e6cd4c3, 0x466482d2, 0x09aa9f07, 0x05d7c214, 0xa2028bd9, 0xd19c12b5,
		0xb94e16de, 0xe883d0cb, 0x4e3c50a2]!
	x1 := chacha20_core(u32input)
	assert x1 == exp_x1
}
