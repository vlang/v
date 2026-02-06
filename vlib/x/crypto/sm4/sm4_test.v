// online tester: https://lzltool.cn/SM4
import x.crypto.sm4

struct SM4Data {
	key    string
	iv     string
	input  string
	output string
}

const sm4data_ecb = [
	SM4Data{
		key:    '0123456789abcdeffedcba9876543210'
		input:  '0123456789abcdeffedcba9876543210'
		output: '681edf34d206965e86b3e94f536e4246'
	},
	SM4Data{
		key:    '0123456789abcdeffedcba9876543210'
		input:  '00112233445566778899aabbccddeeff'
		output: '09325c4853832dcb9337a5984f671b9a'
	},
	SM4Data{
		key:    '456789abcdeffedcba98765432100123'
		input:  '2233445566778899aabbccddeeff0011'
		output: '58ab414d84fb3008b0bee987f97021e6'
	},
	SM4Data{
		key:    '89abcdeffedcba987654321001234567'
		input:  '445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233'
		output: '5937a929a2d9137216c72a28cd9cf6195937a929a2d9137216c72a28cd9cf619'
	},
]

const sm4data_cbc = [
	SM4Data{
		key:    '0123456789abcdeffedcba9876543210'
		iv:     '0123456789abcdeffedcba9876543210'
		input:  '0123456789abcdeffedcba9876543210'
		output: '2677f46b09c122cc975533105bd4a22a'
	},
	SM4Data{
		key:    '0123456789abcdeffedcba9876543210'
		iv:     '2677f46b09c122cc975533105bd4a22a'
		input:  '00112233445566778899aabbccddeeff'
		output: '2e7b41173b42b90189bbcf4eeb5b5145'
	},
	SM4Data{
		key:    '0123456789abcdeffedcba9876543210'
		iv:     '2e7b41173b42b90189bbcf4eeb5b5145'
		input:  '29860013b6b74c7503c524b62ecd52df'
		output: 'cd0cd15a934b3f47476292113d7f35bd'
	},
]

fn test_sm4_ecb() {
	mut key := []u8{}
	mut input := []u8{}
	mut output := []u8{}

	mut cipher := &sm4.SM4Cipher{}

	for data in sm4data_ecb {
		key = '0x${data.key}'.u8_array()
		input = '0x${data.input}'.u8_array()
		output = [u8(0)].repeat(input.len) // output must be exactly the same length as input
		assert key.len == 16
		cipher = sm4.new_cipher(.sm4_encrypt, key)!
		cipher.crypt_ecb(input, mut output)!
		assert output.hex() == data.output

		cipher = sm4.new_cipher(.sm4_decrypt, key)!
		cipher.crypt_ecb(output, mut output)!
		assert output.hex() == data.input
	}
}

fn test_sm4_cbc() {
	mut key := []u8{}
	mut iv := []u8{}
	mut input := []u8{}
	mut output := []u8{}
	mut iv_next := []u8{}

	mut cipher := &sm4.SM4Cipher{}

	// CBC encrypt
	for i, data in sm4data_cbc {
		key = '0x${data.key}'.u8_array()
		iv = '0x${data.iv}'.u8_array()
		input = '0x${data.input}'.u8_array()
		output = [u8(0)].repeat(input.len) // output must be exactly the same length as input
		assert key.len == 16
		assert iv.len == 16
		if i != 0 {
			assert iv_next == iv
		}
		cipher = sm4.new_cipher(.sm4_encrypt, key)!
		iv_next = cipher.crypt_cbc(iv, input, mut output)!
		dump(iv_next.hex())
		assert output.hex() == data.output
	}
	// CBC decrypt
	for i, data in sm4data_cbc {
		key = '0x${data.key}'.u8_array()
		iv = '0x${data.iv}'.u8_array()
		input = '0x${data.output}'.u8_array()
		output = [u8(0)].repeat(input.len) // output must be exactly the same length as input
		assert key.len == 16
		assert iv.len == 16
		if i != 0 {
			assert iv_next == iv
		}
		cipher = sm4.new_cipher(.sm4_decrypt, key)!
		iv_next = cipher.crypt_cbc(iv, input, mut output)!
		dump(iv_next.hex())
		assert output.hex() == data.input
	}
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
	c3.crypt_cbc([u8(0xff)].repeat(22), [u8(0xff)].repeat(16), mut [u8(0xff)].repeat(16)) or {
		fail_flag = true
		assert err.msg() == 'iv length must be exactly 16 bytes'
	}
	assert fail_flag

	// wrong input length test(cbc)
	fail_flag = false
	mut c4 := sm4.new_cipher(.sm4_encrypt, [u8(0xff)].repeat(16))!
	c4.crypt_cbc([u8(0xff)].repeat(16), [u8(0xff)].repeat(111), mut output) or {
		fail_flag = true
		assert err.msg() == 'input must be padded to multiple of 16 bytes'
	}
	assert fail_flag

	// wrong output length test(cbc)
	fail_flag = false
	mut c5 := sm4.new_cipher(.sm4_encrypt, [u8(0xff)].repeat(16))!
	c5.crypt_cbc([u8(0xff)].repeat(16), [u8(0xff)].repeat(16), mut output) or {
		fail_flag = true
		assert err.msg() == 'output must be exactly the same length as input'
	}
	assert fail_flag
}
