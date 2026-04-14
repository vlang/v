module aes

import encoding.hex

struct BlockVector {
	key        string
	plaintext  string
	ciphertext string
}

fn test_sbox_roundtrip() {
	for i in 0 .. 256 {
		b := u8(i)
		assert inv_sub_byte(sub_byte(b)) == b
	}
}

fn test_aes_known_answer_vectors() {
	test_cases := [
		BlockVector{
			key:        '000102030405060708090a0b0c0d0e0f'
			plaintext:  '00112233445566778899aabbccddeeff'
			ciphertext: '69c4e0d86a7b0430d8cdb78070b4c55a'
		},
		BlockVector{
			key:        '000102030405060708090a0b0c0d0e0f1011121314151617'
			plaintext:  '00112233445566778899aabbccddeeff'
			ciphertext: 'dda97ca4864cdfe06eaf70a0ec0d7191'
		},
		BlockVector{
			key:        '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
			plaintext:  '00112233445566778899aabbccddeeff'
			ciphertext: '8ea2b7ca516745bfeafc49904b496089'
		},
	]
	for tc in test_cases {
		key := hex.decode(tc.key) or { panic(err) }
		plaintext := hex.decode(tc.plaintext) or { panic(err) }
		expected_ciphertext := hex.decode(tc.ciphertext) or { panic(err) }
		block := new_cipher(key)
		mut ciphertext := []u8{len: block_size}
		block.encrypt(mut ciphertext, plaintext)
		assert ciphertext == expected_ciphertext
		mut decrypted := []u8{len: block_size}
		block.decrypt(mut decrypted, ciphertext)
		assert decrypted == plaintext
	}
}
