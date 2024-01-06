module chacha20

import encoding.hex

struct BlockCase {
	key     string
	nonce   string
	counter u32
	output  string
}

fn test_chacha20_no_overlap_xor_key_stream() ! {
	for i, t in xorkeystream_testcases {
		key := hex.decode(t.key)!
		nonce := hex.decode(t.nonce)!
		mut cs := new_cipher(key, nonce)!

		input := hex.decode(t.input)!
		mut output := []u8{len: input.len}
		cs.xor_key_stream(mut output, input)
		got := hex.encode(output)

		assert got == t.output

		// decryption back
		// for decryption, we can not use cs.xor_key_stream directly on output bytes
		// internally, Cipher stream has updates the counter, thats differ from encryption phase
		// you can use Cipher instance by set Cipher counter
		plaintext := decrypt(key, nonce, output)!
		assert plaintext == input
	}
}

fn test_chacha20_block_function() ! {
	for val in chacha20.blocks_testcases {
		key_bytes := hex.decode(val.key)!
		nonce_bytes := hex.decode(val.nonce)!
		mut cs := new_cipher(key_bytes, nonce_bytes)!
		cs.set_counter(val.counter)
		cs.chacha20_block()
		exp_bytes := hex.decode(val.output)!

		assert cs.block == exp_bytes
	}
}

fn test_chacha20_simple_block_function() ! {
	key := '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
	key_bytes := hex.decode(key)!

	nonce := '000000090000004a00000000'
	nonce_bytes := hex.decode(nonce)!

	mut block := []u8{len: block_size}
	mut cs := new_cipher(key_bytes, nonce_bytes)!
	cs.set_counter(u32(1))
	cs.chacha20_block()

	expected_raw_bytes := '10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e'
	exp_bytes := hex.decode(expected_raw_bytes)!

	assert cs.block == exp_bytes
}

fn test_chacha20_quarter_round() {
	a, b, c, d := quarter_round(0x11111111, 0x01020304, 0x9b8d6f43, 0x01234567)
	assert a == 0xea2a92f4
	assert b == 0xcb1cf8ce
	assert c == 0x4581472e
	assert d == 0x5881c4bb
}

// test poly1305 key generator as specified in https://datatracker.ietf.org/doc/html/rfc8439#section-2.6.2
fn test_chacha20_onetime_key_gen() ! {
	for i, v in chacha20.otk_cases {
		key := hex.decode(v.key)!
		nonce := hex.decode(v.nonce)!

		otk := hex.decode(v.otk)!
		out := otk_key_gen(key, nonce)!

		assert out == otk
	}
}

struct PolyOtk {
	key   string
	nonce string
	otk   string
}

const otk_cases = [
	// example otk test
	PolyOtk{
		key: '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		nonce: '000000000001020304050607'
		otk: '8ad5a08b905f81cc815040274ab29471a833b637e3fd0da508dbb8e2fdd1a646'
	},
	// Test Vector #1:
	PolyOtk{
		key: '0000000000000000000000000000000000000000000000000000000000000000'
		nonce: '000000000000000000000000'
		otk: '76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7'
	},
	// Test Vector #2:
	PolyOtk{
		key: '0000000000000000000000000000000000000000000000000000000000000001'
		nonce: '000000000000000000000002'
		otk: 'ecfa254f845f647473d3cb140da9e87606cb33066c447b87bc2666dde3fbb739'
	},
	// Test Vector #3:
	PolyOtk{
		key: '1c9240a5eb55d38af333888604f6b5f0473917c1402b80099dca5cbc207075c0'
		nonce: '000000000000000000000002'
		otk: '965e3bc6f9ec7ed9560808f4d229f94b137ff275ca9b3fcbdd59deaad23310ae'
	},
]

const blocks_testcases = [
	// section 2.3.4 https://datatracker.ietf.org/doc/html/rfc8439#section-2.3.2
	BlockCase{
		key: '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
		nonce: '000000090000004a00000000'
		counter: u32(1)
		output: '10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e'
	},
	// https://datatracker.ietf.org/doc/html/rfc8439#appendix-A.1.1
	BlockCase{
		key: '0000000000000000000000000000000000000000000000000000000000000000'
		nonce: '000000000000000000000000'
		counter: u32(0)
		output: '76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7da41597c5157488d7724e03fb8d84a376a43b8f41518a11cc387b669b2ee6586'
	},
	// #appendix-A.1.2
	BlockCase{
		key: '0000000000000000000000000000000000000000000000000000000000000000'
		nonce: '000000000000000000000000'
		counter: u32(1)
		output: '9f07e7be5551387a98ba977c732d080dcb0f29a048e3656912c6533e32ee7aed29b721769ce64e43d57133b074d839d531ed1f28510afb45ace10a1f4b794d6f'
	},
	//#appendix-A.1.3
	BlockCase{
		key: '0000000000000000000000000000000000000000000000000000000000000001'
		nonce: '000000000000000000000000'
		counter: u32(1)
		output: '3aeb5224ecf849929b9d828db1ced4dd832025e8018b8160b82284f3c949aa5a8eca00bbb4a73bdad192b5c42f73f2fd4e273644c8b36125a64addeb006c13a0'
	},
	// #appendix-A.1.4
	BlockCase{
		key: '00ff000000000000000000000000000000000000000000000000000000000000'
		nonce: '000000000000000000000000'
		counter: u32(2)
		output: '72d54dfbf12ec44b362692df94137f328fea8da73990265ec1bbbea1ae9af0ca13b25aa26cb4a648cb9b9d1be65b2c0924a66c54d545ec1b7374f4872e99f096'
	},
	// #appendix-A.1.5
	BlockCase{
		key: '0000000000000000000000000000000000000000000000000000000000000000'
		nonce: '000000000000000000000002'
		counter: u32(0)
		output: 'c2c64d378cd536374ae204b9ef933fcd1a8b2288b3dfa49672ab765b54ee27c78a970e0e955c14f3a88e741b97c286f75f8fc299e8148362fa198a39531bed6d'
	},
]

struct EncryptionCase {
	title     string
	key       string
	nonce     string
	counter   u32
	plaintext string
	output    string
}

fn test_chacha20_cipher_encrypt() ! {
	for c in chacha20.encryption_test_cases {
		key_bytes := hex.decode(c.key)!
		nonce_bytes := hex.decode(c.nonce)!
		plaintext_bytes := hex.decode(c.plaintext)!

		mut cs := new_cipher(key_bytes, nonce_bytes)!
		cs.set_counter(c.counter)

		mut output := []u8{len: plaintext_bytes.len}
		cs.xor_key_stream(mut output, plaintext_bytes)

		expected := hex.decode(c.output)!
		assert output == expected
	}
}

fn test_chacha20_cipher_decrypt() ! {
	for c in chacha20.encryption_test_cases {
		key_bytes := hex.decode(c.key)!
		nonce_bytes := hex.decode(c.nonce)!

		ciphertext := hex.decode(c.output)!
		mut cs := new_cipher(key_bytes, nonce_bytes)!
		cs.set_counter(c.counter)

		mut output := []u8{len: ciphertext.len}
		cs.xor_key_stream(mut output, ciphertext)

		expected_decrypted_message := hex.decode(c.plaintext)!
		assert output == expected_decrypted_message
	}
}

const encryption_test_cases = [
	// core test
	EncryptionCase{
		title: 'core test'
		key: '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
		nonce: '000000000000004a00000000'
		counter: u32(1)
		plaintext: '4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e'
		output: '6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0bf91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d807ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab77937365af90bbf74a35be6b40b8eedf2785e42874d'
	},
	// https://datatracker.ietf.org/doc/html/rfc8439#appendix-A.2
	// Appendix A.2.1
	EncryptionCase{
		title: 'A.2.1'
		key: '0000000000000000000000000000000000000000000000000000000000000000'
		nonce: '000000000000000000000000'
		counter: u32(0)
		plaintext: '00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000'
		output: '76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7da41597c5157488d7724e03fb8d84a376a43b8f41518a11cc387b669b2ee6586'
	},
	// Appendix A.2.2
	EncryptionCase{
		title: 'A.2.2'
		key: '0000000000000000000000000000000000000000000000000000000000000001'
		nonce: '000000000000000000000002'
		counter: u32(1)
		plaintext: '416e79207375626d697373696f6e20746f20746865204945544620696e74656e6465642062792074686520436f6e7472696275746f7220666f72207075626c69636174696f6e20617320616c6c206f722070617274206f6620616e204945544620496e7465726e65742d4472616674206f722052464320616e6420616e792073746174656d656e74206d6164652077697468696e2074686520636f6e74657874206f6620616e204945544620616374697669747920697320636f6e7369646572656420616e20224945544620436f6e747269627574696f6e222e20537563682073746174656d656e747320696e636c756465206f72616c2073746174656d656e747320696e20494554462073657373696f6e732c2061732077656c6c206173207772697474656e20616e6420656c656374726f6e696320636f6d6d756e69636174696f6e73206d61646520617420616e792074696d65206f7220706c6163652c207768696368206172652061646472657373656420746f'
		output: 'a3fbf07df3fa2fde4f376ca23e82737041605d9f4f4f57bd8cff2c1d4b7955ec2a97948bd3722915c8f3d337f7d370050e9e96d647b7c39f56e031ca5eb6250d4042e02785ececfa4b4bb5e8ead0440e20b6e8db09d881a7c6132f420e52795042bdfa7773d8a9051447b3291ce1411c680465552aa6c405b7764d5e87bea85ad00f8449ed8f72d0d662ab052691ca66424bc86d2df80ea41f43abf937d3259dc4b2d0dfb48a6c9139ddd7f76966e928e635553ba76c5c879d7b35d49eb2e62b0871cdac638939e25e8a1e0ef9d5280fa8ca328b351c3c765989cbcf3daa8b6ccc3aaf9f3979c92b3720fc88dc95ed84a1be059c6499b9fda236e7e818b04b0bc39c1e876b193bfe5569753f88128cc08aaa9b63d1a16f80ef2554d7189c411f5869ca52c5b83fa36ff216b9c1d30062bebcfd2dc5bce0911934fda79a86f6e698ced759c3ff9b6477338f3da4f9cd8514ea9982ccafb341b2384dd902f3d1ab7ac61dd29c6f21ba5b862f3730e37cfdc4fd806c22f221'
	},
	// Appendix A.2.3
	EncryptionCase{
		title: 'A.2.3'
		key: '1c9240a5eb55d38af333888604f6b5f0473917c1402b80099dca5cbc207075c0'
		nonce: '000000000000000000000002'
		counter: u32(42)
		plaintext: '2754776173206272696c6c69672c20616e642074686520736c6974687920746f7665730a446964206779726520616e642067696d626c6520696e2074686520776162653a0a416c6c206d696d737920776572652074686520626f726f676f7665732c0a416e6420746865206d6f6d65207261746873206f757467726162652e'
		output: '62e6347f95ed87a45ffae7426f27a1df5fb69110044c0d73118effa95b01e5cf166d3df2d721caf9b21e5fb14c616871fd84c54f9d65b283196c7fe4f60553ebf39c6402c42234e32a356b3e764312a61a5532055716ead6962568f87d3f3f7704c6a8d1bcd1bf4d50d6154b6da731b187b58dfd728afa36757a797ac188d1'
	},
]
