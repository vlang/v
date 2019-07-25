// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
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
	if ciphertext.len < aes.BlockSize {
		panic('ciphertext too short')
	}
	// iv := ciphertext[:aes.BlockSize]
	// ciphertext = ciphertext[aes.BlockSize:]
	iv := ciphertext.left(aes.BlockSize)
	ciphertext = ciphertext.right(aes.BlockSize)
	// CBC mode always works in whole blocks.
	if ciphertext.len%aes.BlockSize != 0 {
		panic('ciphertext is not a multiple of the block size')
	}
	mode := aes.new_cbc(block, iv)
	mode.encrypt_blocks(ciphertext, ciphertext)

	assert ciphertext.hex() == 'C210459B514668DDC44674885E4979215265A6C44431A248421254EF357A8C2A308A8BDDF5623AF9DF91737562041CF1'
}
