// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.aes

fn test_aes() {
	key := '6368616e676520746869732070617373'.bytes()
	mut ciphertext := '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'.bytes()
	block := aes.new_cipher(key)!

	block.encrypt(mut ciphertext, ciphertext.clone())
	assert ciphertext.hex() == '05d1737fe0a7c12088ff4c94d62ccbd9353361393663383562306636623033616339373932653065373537663836396363613330366264336362613163363262'

	block.decrypt(mut ciphertext, ciphertext.clone())

	assert ciphertext.bytestr() == '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'
	println('test_aes ok')
}

fn test_new_cipher_invalid_key_returns_error() {
	// keys must be 16, 24 or 32 bytes; anything else should return an error
	// (instead of panicking, which is hard to handle by callers).
	for bad_len in [0, 1, 8, 15, 17, 33] {
		bad_key := []u8{len: bad_len}
		if _ := aes.new_cipher(bad_key) {
			assert false, 'expected an error for key size ${bad_len}'
		} else {
			assert err.msg().contains('invalid key size')
		}
	}
}

fn test_new_cipher_valid_key_sizes() {
	for good_len in [16, 24, 32] {
		good_key := []u8{len: good_len}
		aes.new_cipher(good_key) or { assert false, 'unexpected error for key size ${good_len}' }
	}
}
