// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.aes
import crypto.cipher

fn test_aes_cbc() {
	key := '6368616e676520746869732070617373'.bytes()
	iv := '1234567890123456'.bytes()
	str := '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'

	mut src := str.bytes()

	aes_cbc_en(mut src, key, iv)
	assert src.hex() == '20f36e39e6b73a040d3d39445d0282369033693076d1129a68a3364c00a81c508ec06ab94ff5104c321939fbfb9313c549fd68df100a8a23e8006a863733a8fd'

	aes_cbc_de(mut src, key, iv)
	assert src.bytestr() == str
	println('test_aes_cbc ok')
}

fn aes_cbc_en(mut src []byte, key []byte, iv []byte) {
	block := aes.new_cipher(key)
	mut mode := cipher.new_cbc(block, iv)
	mode.encrypt_blocks(mut src, src.clone())
}

fn aes_cbc_de(mut src []byte, key []byte, iv []byte) {
	block := aes.new_cipher(key)
	mut mode := cipher.new_cbc(block, iv)
	mode.decrypt_blocks(mut src, src.clone())
}
