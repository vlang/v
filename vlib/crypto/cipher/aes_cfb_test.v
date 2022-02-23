import crypto.aes
import crypto.cipher

fn test_aes_cfb() {
	key := '6368616e676520746869732070617373'.bytes()
	iv := '1234567890123456'.bytes()
	str := '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'

	mut src := str.bytes()

	aes_cfb_en(mut src, key, iv)
	assert src.hex() == '04380c1470ab2d8a0b3f9b4c1949b8ac57dfecca20ab539cd9862a262857ed3e4be1b1bb1d590f3c9eb760ef3d0c202b38e79ea53efbe4a3334f0a872e82f208'

	aes_cfb_de(mut src, key, iv)
	assert src.bytestr() == str
	println('test_aes_cfb ok')
}

fn aes_cfb_en(mut src []byte, key []byte, iv []byte) {
	block := aes.new_cipher(key)
	mode := cipher.new_cfb_encrypter(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn aes_cfb_de(mut src []byte, key []byte, iv []byte) {
	block := aes.new_cipher(key)
	mut mode := cipher.new_cfb_decrypter(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}
