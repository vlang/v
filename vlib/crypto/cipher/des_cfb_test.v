import crypto.des
import crypto.cipher

const (
	key = '123456789012345678901234'.bytes()
	iv  = 'abcdegfh'.bytes()
	str = '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'
)

fn test_triple_des_cfb() {
	mut src := str.bytes()

	triple_des_cfb_en(mut src, key, iv)
	assert src.hex() == '00d963619a67c84ef3e300e35c4d03fecbb3c4b1a2ee1abbff706dbe6da94ab5803359d1a6ac2b9cda703dbc32071abb2b1682fccdc92903b4e06187512e6d46'

	triple_des_cfb_de(mut src, key, iv)
	assert src.bytestr() == str
	println('test_triple_des_cfb ok')
}

fn test_des_cfb() {
	mut src := str.bytes()

	des_cfb_en(mut src, key[..8], iv)
	assert src.hex() == '2743e2164b8604562f4ab73dd3d1bccb1fc08e77f34560b920ec8cce5c6a110ea1fcdc2a7ddd812e35387435de2985f6e636893db25a9d0683748edc145e1ef0'

	des_cfb_de(mut src, key[..8], iv)
	assert src.bytestr() == str
	println('test_des_cfb ok')
}

fn des_cfb_en(mut src []u8, key []u8, iv []u8) {
	block := des.new_cipher(key)
	mut mode := cipher.new_cfb_encrypter(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn des_cfb_de(mut src []u8, key []u8, iv []u8) {
	block := des.new_cipher(key)
	mut mode := cipher.new_cfb_decrypter(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn triple_des_cfb_en(mut src []u8, key []u8, iv []u8) {
	block := des.new_triple_des_cipher(key)
	mut mode := cipher.new_cfb_encrypter(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn triple_des_cfb_de(mut src []u8, key []u8, iv []u8) {
	block := des.new_triple_des_cipher(key)
	mut mode := cipher.new_cfb_decrypter(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}
