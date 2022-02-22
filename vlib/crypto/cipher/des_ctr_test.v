import crypto.des
import crypto.cipher

const (
	key = '123456789012345678901234'.bytes()
	iv  = 'abcdegfh'.bytes()
	str = '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'
)

fn test_triple_des_ctr() {
	mut src := str.bytes()

	triple_des_ctr_en(mut src, key, iv)
	assert src.hex() == '00d963619a67c84e5aa688174f259f4615768a516ec3fd10c98848f8626120db27fd2932130a4ba5e90acd5c347583bd4705770d41465b1ba11b4a523f449beb'

	triple_des_ctr_de(mut src, key, iv)
	assert src.bytestr() == str
	println('test_triple_des_ctr ok')
}

fn test_des_ctr() {
	mut src := str.bytes()

	des_ctr_en(mut src, key[..8], iv)
	assert src.hex() == '2743e2164b8604565f34bb120eb5dcd0f9db5f9a6498bdb678497c68c3cdda70500f4028159903d77d6b2a7ee4eb710c60e0f816deab4d919287c6a0cd9d2cd3'

	des_ctr_de(mut src, key[..8], iv)
	assert src.bytestr() == str
	println('test_des_ctr ok')
}

fn des_ctr_en(mut src []byte, key []byte, iv []byte) {
	block := des.new_cipher(key)
	mode := cipher.new_ctr(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn des_ctr_de(mut src []byte, key []byte, iv []byte) {
	block := des.new_cipher(key)
	mode := cipher.new_ctr(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn triple_des_ctr_en(mut src []byte, key []byte, iv []byte) {
	block := des.new_triple_des_cipher(key)
	mode := cipher.new_ctr(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}

fn triple_des_ctr_de(mut src []byte, key []byte, iv []byte) {
	block := des.new_triple_des_cipher(key)
	mode := cipher.new_ctr(block, iv)
	mode.xor_key_stream(mut src, src.clone())
}
