// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.aes

fn test_crypto_aes() {
	// TEST CBC
	key := '6368616e676520746869732070617373'.bytes()
	mut ciphertext := '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'.bytes()
	block := aes.new_cipher(key)
	// The IV needs to be unique, but not secure. Therefore it's common to
	// include it at the beginning of the ciphertext.
	if ciphertext.len < aes.block_size {
		panic('ciphertext too short')
	}
	iv := ciphertext[..aes.block_size]
	ciphertext = ciphertext[aes.block_size..]
	// CBC mode always works in whole blocks.
	if ciphertext.len % aes.block_size != 0 {
		panic('ciphertext is not a multiple of the block size')
	}
	mode := aes.new_cbc(block, iv)
	cipher_clone := ciphertext.clone()
	mode.encrypt_blocks(mut ciphertext, cipher_clone)
	assert ciphertext.hex() ==
		'c210459b514668ddc44674885e4979215265a6c44431a248421254ef357a8c2a308a8bddf5623af9df91737562041cf1'
	println('ok')
}
