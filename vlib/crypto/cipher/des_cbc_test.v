import crypto.des
import crypto.cipher

const (
	key = '123456789012345678901234'.bytes()
	iv  = 'abcdegfh'.bytes()
	str = '73c86d43a9d700a253a96c85b0f6b03ac9792e0e757f869cca306bd3cba1c62b'
)

fn test_triple_des_cbc() {
	mut src := str.bytes()

	triple_des_cbc_en(mut src, key, iv)
	assert src.hex() == '59d657007dc96062e8fffd574afda480ddba21fa06337ac5676eb4e40db256f2ab31ed442c0e4a82ef59b96292d24902c86b20c50bd8506e387775ca58f8c4fe'

	triple_des_cbc_de(mut src, key, iv)
	assert src.bytestr() == str
	println('test_triple_des_cbc ok')
}

fn test_des_cbc() {
	mut src := str.bytes()

	des_cbc_en(mut src, key[..8], iv)
	assert src.hex() == '198f94ca5989900ce73b26c3ce0005fa747b74d81e8cc5d529f96c1a2e7748d39f9900b9049cbfda35ef720d495b134f4f7dd2d7d3d910af488cdccd27d9f057'

	des_cbc_de(mut src, key[..8], iv)
	assert src.bytestr() == str
	println('test_des_cbc ok')
}

fn des_cbc_en(mut src []byte, key []byte, iv []byte) {
	block := des.new_cipher(key)
	mut mode := cipher.new_cbc(block, iv)
	mode.encrypt_blocks(mut src, src.clone())
}

fn des_cbc_de(mut src []byte, key []byte, iv []byte) {
	block := des.new_cipher(key)
	mut mode := cipher.new_cbc(block, iv)
	mode.decrypt_blocks(mut src, src.clone())
}

fn triple_des_cbc_en(mut src []byte, key []byte, iv []byte) {
	block := des.new_triple_des_cipher(key)
	mut mode := cipher.new_cbc(block, iv)
	mode.encrypt_blocks(mut src, src.clone())
}

fn triple_des_cbc_de(mut src []byte, key []byte, iv []byte) {
	block := des.new_triple_des_cipher(key)
	mut mode := cipher.new_cbc(block, iv)
	mode.decrypt_blocks(mut src, src.clone())
}
