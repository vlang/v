import encoding.base32

// TODO: add more tests

fn test_encode_and_decode() {
	input := 'hello v'

	encoded := base32.encode_string_to_string(input)
	assert encoded == 'NBSWY3DPEB3A===='

	decoded := base32.decode_string_to_string(encoded) or { panic('error decoding: ${err}') }
	assert decoded == input

	encoder_no_padding := base32.new_std_encoding_with_padding(base32.no_padding)
	encoded2 := encoder_no_padding.encode_string_to_string(input)
	assert encoded2 == 'NBSWY3DPEB3A'

	decoded2 := encoder_no_padding.decode_string_to_string(encoded2) or {
		panic('error decoding: ${err}')
	}
	assert decoded2 == input
}

fn test_unpadded_invalid_final_quantum_lengths() {
	for alphabet in [base32.std_alphabet, base32.hex_alphabet] {
		enc := base32.new_encoding_with_padding(alphabet, base32.no_padding)
		for prefix_length in [0, 8, 16] {
			for tail_length in [1, 3, 6] {
				input := alphabet[0].ascii_str().repeat(prefix_length + tail_length)
				if decoded := enc.decode_string(input) {
					assert false, 'accepted ${input}: ${decoded}'
				} else {
					assert err.msg() == 'illegal base32 data at input byte ${prefix_length}'
				}
			}
		}
	}
}

fn test_unpadded_valid_final_quantum_lengths() {
	for alphabet in [base32.std_alphabet, base32.hex_alphabet] {
		enc := base32.new_encoding_with_padding(alphabet, base32.no_padding)
		for length in 0 .. 16 {
			input := []u8{len: length, init: u8(index * 17 + 5)}
			assert enc.decode(enc.encode_string_to_string(input.bytestr()).bytes())! == input
		}
	}
}
