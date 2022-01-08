// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.aes

fn test_aes() {
	key := '6368616e676520746869732070617373'.bytes()
	mut ciphertext := '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'.bytes()
	block := aes.new_cipher(key)

	block.encrypt(mut ciphertext, ciphertext.clone())
	assert ciphertext.hex() == '05d1737fe0a7c12088ff4c94d62ccbd9353361393663383562306636623033616339373932653065373537663836396363613330366264336362613163363262'

	block.decrypt(mut ciphertext, ciphertext.clone())

	assert ciphertext.bytestr() == '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'
	println('test_aes ok')
}
