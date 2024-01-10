import crypto.aes
import crypto.cipher

fn test_aes_ctr() {
	key := '6368616e676520746869732070617373'.bytes()
	iv := '1234567890123456'.bytes()
	str := '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'

	mut src := str.bytes()

	aes_ctr_en(mut src, key, iv)
	assert src.hex() == '04380c1470ab2d8a0b3f9b4c1949b8ace811f22bc0a455a8946a3ea7a03e9f8440820f273ce749e955e4adb9e8f6ffde82bae507e0b6164b531d3eeb32f5beb5'

	aes_ctr_de(mut src, key, iv)
	assert src.bytestr() == str
	println('test_aes_ctr ok')
}

fn aes_ctr_en(mut src []u8, key []u8, iv []u8) {
	block := aes.new_cipher(key)
	mode := cipher.new_ctr(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn aes_ctr_de(mut src []u8, key []u8, iv []u8) {
	block := aes.new_cipher(key)
	mode := cipher.new_ctr(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}
