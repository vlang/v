module base32

fn test_validate_encoding_alphabet() {
	validate_encoding_alphabet(std_alphabet, std_padding)!
	for alphabet in [std_alphabet[..31], std_alphabet.repeat(2), []u8{len: 32, init: `A`}] {
		mut rejected := false
		validate_encoding_alphabet(alphabet, std_padding) or { rejected = true }
		assert rejected, 'accepted malformed alphabet'
	}
	for ch in [u8(`\r`), `\n`, std_padding] {
		mut alphabet := std_alphabet.clone()
		alphabet[0] = ch
		mut rejected := false
		validate_encoding_alphabet(alphabet, std_padding) or { rejected = true }
		assert rejected, 'accepted forbidden alphabet byte ${ch}'
	}
}

fn test_high_byte_custom_alphabet_round_trip() {
	alphabet := []u8{len: 32, init: u8(0x80 + index)}
	enc := new_encoding_with_padding(alphabet, no_padding)
	input := 'hello'.bytes()
	assert enc.decode(enc.encode(input))! == input
}

fn test_encoding_keeps_a_copy_of_its_alphabet() {
	mut alphabet := std_alphabet.clone()
	enc := new_encoding(alphabet)
	input := [u8(0)]
	alphabet[0] = `!`

	assert enc.encode_to_string(input) == 'AA======'
	assert enc.decode_string('AA======')! == input
}
