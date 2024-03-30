// online tester: https://lzltool.cn/SM4
import x.crypto.sm4

fn test_sm4_ecb() ! {
	mut key := [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76,
		0x54, 0x32, 0x10]
	mut input := [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76,
		0x54, 0x32, 0x10]
	mut output := []u8{len: 16}

	// key = 0123456789abcdeffedcba9876543210
	// plaintext = 0123456789abcdeffedcba9876543210
	// ciphertext = 681edf34d206965e86b3e94f536e4246
	mut c1 := sm4.new_cipher(.sm4_encrypt, key)!
	c1.crypt_ecb(input, mut output)!
	assert output.hex() == '681edf34d206965e86b3e94f536e4246'
	mut d1 := sm4.new_cipher(.sm4_decrypt, key)!
	d1.crypt_ecb(output, mut output)!
	assert output.hex() == '0123456789abcdeffedcba9876543210'

	// key = 0123456789abcdeffedcba9876543210
	// plaintext = 00112233445566778899aabbccddeeff
	// ciphertext = 09325c4853832dcb9337a5984f671b9a
	key = [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54,
		0x32, 0x10]
	input = [u8(0x00), 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc,
		0xdd, 0xee, 0xff]
	output = []u8{len: 16}
	mut c2 := sm4.new_cipher(.sm4_encrypt, key)!
	c2.crypt_ecb(input, mut output)!
	assert output.hex() == '09325c4853832dcb9337a5984f671b9a'
	mut d2 := sm4.new_cipher(.sm4_decrypt, key)!
	d2.crypt_ecb(output, mut output)!
	assert output.hex() == '00112233445566778899aabbccddeeff'

	// key = 456789abcdeffedcba98765432100123
	// plaintext = 2233445566778899aabbccddeeff0011
	// ciphertext = 58ab414d84fb3008b0bee987f97021e6
	key = [u8(0x45), 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10,
		0x01, 0x23]
	input = [u8(0x22), 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee,
		0xff, 0x00, 0x11]
	output = []u8{len: 16}
	mut c3 := sm4.new_cipher(.sm4_encrypt, key)!
	c3.crypt_ecb(input, mut output)!
	assert output.hex() == '58ab414d84fb3008b0bee987f97021e6'
	mut d3 := sm4.new_cipher(.sm4_decrypt, key)!
	d3.crypt_ecb(output, mut output)!
	assert output.hex() == '2233445566778899aabbccddeeff0011'

	// key = 89abcdeffedcba987654321001234567
	// plaintext = 445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233
	// ciphertext = 5937a929a2d9137216c72a28cd9cf6195937a929a2d9137216c72a28cd9cf619
	key = [u8(0x89), 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10, 0x01, 0x23,
		0x45, 0x67]
	input = [u8(0x44), 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00,
		0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff,
		0x00, 0x11, 0x22, 0x33]
	output = []u8{len: 32}
	mut c4 := sm4.new_cipher(.sm4_encrypt, key)!
	c4.crypt_ecb(input, mut output)!
	assert output.hex() == '5937a929a2d9137216c72a28cd9cf6195937a929a2d9137216c72a28cd9cf619'
	mut d4 := sm4.new_cipher(.sm4_decrypt, key)!
	d4.crypt_ecb(output, mut output)!
	assert output.hex() == '445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233'
}

fn test_sm4_cbc() ! {
	mut key := [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76,
		0x54, 0x32, 0x10]
	mut iv := [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76,
		0x54, 0x32, 0x10]
	mut orig_iv := iv.clone()
	mut input := [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76,
		0x54, 0x32, 0x10]
	mut output := []u8{len: 16}

	// key = 0123456789abcdeffedcba9876543210
	// iv = 0123456789abcdeffedcba9876543210
	// plaintext = 0123456789abcdeffedcba9876543210
	// ciphertext = 2677f46b09c122cc975533105bd4a22a
	mut c1 := sm4.new_cipher(.sm4_encrypt, key)!
	c1.crypt_cbc(mut iv, input, mut output)!
	assert output.hex() == '2677f46b09c122cc975533105bd4a22a'
	iv = orig_iv.clone()
	mut d1 := sm4.new_cipher(.sm4_decrypt, key)!
	d1.crypt_cbc(mut iv, output, mut output)!
	assert output.hex() == '0123456789abcdeffedcba9876543210'

	// key = 0123456789abcdeffedcba9876543210
	// iv = 112233445566778899aabbccddeeff00
	// plaintext = 00112233445566778899aabbccddeeff
	// ciphertext = c3c0fffdcaac88ed63672b2b28a84e80
	key = [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54,
		0x32, 0x10]
	iv = [u8(0x11), 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee,
		0xff, 0x00]
	orig_iv = iv.clone()
	input = [u8(0x00), 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc,
		0xdd, 0xee, 0xff]
	output = []u8{len: 16}
	mut c2 := sm4.new_cipher(.sm4_encrypt, key)!
	c2.crypt_cbc(mut iv, input, mut output)!
	assert output.hex() == 'c3c0fffdcaac88ed63672b2b28a84e80'
	iv = orig_iv.clone()
	mut d2 := sm4.new_cipher(.sm4_decrypt, key)!
	d2.crypt_cbc(mut iv, output, mut output)!
	assert output.hex() == '00112233445566778899aabbccddeeff'

	// key = 456789abcdeffedcba98765432100123
	// iv = 33445566778899aabbccddeeff001122
	// plaintext = 2233445566778899aabbccddeeff0011
	// ciphertext = 63dfbfc357c2e040b529c692ec916c6b
	key = [u8(0x45), 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10,
		0x01, 0x23]
	iv = [u8(0x33), 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00,
		0x11, 0x22]
	orig_iv = iv.clone()
	input = [u8(0x22), 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee,
		0xff, 0x00, 0x11]
	output = []u8{len: 16}
	mut c3 := sm4.new_cipher(.sm4_encrypt, key)!
	c3.crypt_cbc(mut iv, input, mut output)!
	assert output.hex() == '63dfbfc357c2e040b529c692ec916c6b'
	iv = orig_iv.clone()
	mut d3 := sm4.new_cipher(.sm4_decrypt, key)!
	d3.crypt_cbc(mut iv, output, mut output)!
	assert output.hex() == '2233445566778899aabbccddeeff0011'

	// key = 89abcdeffedcba987654321001234567
	// iv = 48bb57369d2020c66ded73415bfab211
	// plaintext = 445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233
	// ciphertext = 2aac9d5427ddca8f2ef2eaa175012bcb66a1fd8df190a7db9f053f0ee40d79b5
	key = [u8(0x89), 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10, 0x01, 0x23,
		0x45, 0x67]
	iv = [u8(0x48), 0xbb, 0x57, 0x36, 0x9d, 0x20, 0x20, 0xc6, 0x6d, 0xed, 0x73, 0x41, 0x5b, 0xfa,
		0xb2, 0x11]
	orig_iv = iv.clone()
	input = [u8(0x44), 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00,
		0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff,
		0x00, 0x11, 0x22, 0x33]
	output = []u8{len: 32}
	mut c4 := sm4.new_cipher(.sm4_encrypt, key)!
	c4.crypt_cbc(mut iv, input, mut output)!
	assert output.hex() == '2aac9d5427ddca8f2ef2eaa175012bcb66a1fd8df190a7db9f053f0ee40d79b5'
	iv = orig_iv.clone()
	mut d4 := sm4.new_cipher(.sm4_decrypt, key)!
	d4.crypt_cbc(mut iv, output, mut output)!
	assert output.hex() == '445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233'
}

fn test_sm4_wrong_length() ! {
	// wrong key length test
	mut fail_flag := false
	sm4.new_cipher(.sm4_encrypt, [u8(0xff)].repeat(33)) or {
		fail_flag = true
		assert err.msg() == 'SM4 only support 128bit key length'
	}
	assert fail_flag

	fail_flag = false
	sm4.new_cipher(.sm4_decrypt, [u8(0xff)].repeat(33)) or {
		fail_flag = true
		assert err.msg() == 'SM4 only support 128bit key length'
	}
	assert fail_flag

	// wrong input length test(ecb)
	mut output := []u8{len: 32}
	fail_flag = false
	mut c1 := sm4.new_cipher(.sm4_encrypt, [u8(0xff)].repeat(16))!
	c1.crypt_ecb([u8(0xff)].repeat(111), mut output) or {
		fail_flag = true
		assert err.msg() == 'input must be padded to multiple of 16 bytes'
	}
	assert fail_flag

	// wrong output length test(ecb)
	fail_flag = false
	mut c2 := sm4.new_cipher(.sm4_encrypt, [u8(0xff)].repeat(16))!
	c2.crypt_ecb([u8(0xff)].repeat(16), mut output) or {
		fail_flag = true
		assert err.msg() == 'output must be exactly the same length as input'
	}
	assert fail_flag

	// wrong iv length test(cbc)
	fail_flag = false
	mut c3 := sm4.new_cipher(.sm4_encrypt, [u8(0xff)].repeat(16))!
	c3.crypt_cbc(mut [u8(0xff)].repeat(22), [u8(0xff)].repeat(16), mut [u8(0xff)].repeat(16)) or {
		fail_flag = true
		assert err.msg() == 'iv length must be exactly 16 bytes'
	}
	assert fail_flag

	// wrong input length test(cbc)
	fail_flag = false
	mut c4 := sm4.new_cipher(.sm4_encrypt, [u8(0xff)].repeat(16))!
	c4.crypt_cbc(mut [u8(0xff)].repeat(16), [u8(0xff)].repeat(111), mut output) or {
		fail_flag = true
		assert err.msg() == 'input must be padded to multiple of 16 bytes'
	}
	assert fail_flag

	// wrong output length test(cbc)
	fail_flag = false
	mut c5 := sm4.new_cipher(.sm4_encrypt, [u8(0xff)].repeat(16))!
	c5.crypt_cbc(mut [u8(0xff)].repeat(16), [u8(0xff)].repeat(16), mut output) or {
		fail_flag = true
		assert err.msg() == 'output must be exactly the same length as input'
	}
	assert fail_flag
}
