import encoding.base32

// TODO: add more tests

fn test_encode_and_decode() {
	input := 'hello v'

	encoded := base32.encode_string_to_string(input)
	assert encoded == 'NBSWY3DPEB3A===='

	decoded := base32.decode_string_to_string(encoded) or { panic('error decoding: $err') }
	assert decoded == input

	encoder_no_padding := base32.new_std_encoding_with_padding(base32.no_padding)
	encoded2 := encoder_no_padding.encode_string_to_string(input)
	assert encoded2 == 'NBSWY3DPEB3A'

	decoded2 := encoder_no_padding.decode_string_to_string(encoded2) or {
		panic('error decoding: $err')
	}
	assert decoded2 == input
}
