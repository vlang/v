import crypto.des

const (
	key = '123456789012345678901234'.bytes()
	iv  = 'abcdegfh'.bytes()
	str = 'aaaaaaaa'
)

fn test_triple_des() {
	mut src := str.bytes()

	triple_des_en(mut src, key, iv)
	assert src.hex() == '45902cf00aa1df46'

	triple_des_de(mut src, key, iv)
	assert src.bytestr() == str
	println('test_triple_des ok')
}

fn test_des() {
	mut src := str.bytes()

	des_en(mut src, key[..8], iv)
	assert src.hex() == '72dca13c37223cf0'

	des_de(mut src, key[..8], iv)
	assert src.bytestr() == str

	println('test_des ok')
}

fn des_en(mut src []u8, key []u8, iv []u8) {
	block := des.new_cipher(key)
	block.encrypt(mut src, src.clone())
}

fn des_de(mut src []u8, key []u8, iv []u8) {
	block := des.new_cipher(key)
	block.decrypt(mut src, src.clone())
}

fn triple_des_en(mut src []u8, key []u8, iv []u8) {
	block := des.new_triple_des_cipher(key)
	block.encrypt(mut src, src.clone())
}

fn triple_des_de(mut src []u8, key []u8, iv []u8) {
	block := des.new_triple_des_cipher(key)
	inbuf := src.clone()
	block.decrypt(mut src, inbuf)
}
