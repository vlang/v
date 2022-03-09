import crypto.des
import crypto.cipher

const (
	key = '123456789012345678901234'.bytes()
	iv  = 'abcdegfh'.bytes()
	str = '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'
)

fn test_triple_des_ofb() {
	mut src := str.bytes()

	triple_des_ofb_en(mut src, key, iv)
	assert src.hex() == '00d963619a67c84e46741a47e972e407121885af78b8ef64a9b835366e5314b8031940a9c33a4c0e1ce3b417c9fd04c3f99f31821ebfc9f8cb9e80cc50cfd91e'

	triple_des_ofb_de(mut src, key, iv)
	assert src.bytestr() == str
	println('test_triple_des_ofb ok')
}

fn test_des_ofb() {
	mut src := str.bytes()

	des_ofb_en(mut src, key[..8], iv)
	assert src.hex() == '2743e2164b860456c1313fb9b1196a70bb217dfad57be81cb10f368dd1ee13b06bb776eb52e0b4f6b1af32f44b8f094cfd3c0892021a2aa93f6a9e2139ba26f3'

	des_ofb_de(mut src, key[..8], iv)
	assert src.bytestr() == str
	println('test_des_ofb ok')
}

fn des_ofb_en(mut src []byte, key []byte, iv []byte) {
	block := des.new_cipher(key)
	mut mode := cipher.new_ofb(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn des_ofb_de(mut src []byte, key []byte, iv []byte) {
	block := des.new_cipher(key)
	mut mode := cipher.new_ofb(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn triple_des_ofb_en(mut src []byte, key []byte, iv []byte) {
	block := des.new_triple_des_cipher(key)
	mut mode := cipher.new_ofb(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn triple_des_ofb_de(mut src []byte, key []byte, iv []byte) {
	block := des.new_triple_des_cipher(key)
	mut mode := cipher.new_ofb(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}
