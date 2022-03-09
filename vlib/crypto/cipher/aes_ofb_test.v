import crypto.aes
import crypto.cipher

fn test_aes_ofb() {
	key := '6368616e676520746869732070617373'.bytes()
	iv := '1234567890123456'.bytes()
	str := '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'

	mut src := str.bytes()

	aes_ofb_en(mut src, key, iv)
	assert src.hex() == '04380c1470ab2d8a0b3f9b4c1949b8aca120db226fa0a848236f9904f786dfc39bc433b5d3870d1cfdef36debdee5e6f39b0e8e9462b0151d61e822944181b51'
	dump(src.hex())
	mut plaintext := src.clone()
	aes_ofb_de(mut plaintext, key, iv)
	assert plaintext.bytestr() == str
	dump(plaintext.bytestr())
	println('test_aes_ofb ok')
}

fn aes_ofb_en(mut src []byte, key []byte, iv []byte) {
	block := aes.new_cipher(key)
	mut mode := cipher.new_ofb(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn aes_ofb_de(mut src []byte, key []byte, iv []byte) {
	block := aes.new_cipher(key)
	mut mode := cipher.new_ofb(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}
