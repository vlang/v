module ssa

fn test_native_c_string_decoder_preserves_bytes_and_escape_sequences() {
	assert decode_native_c_string('literal') == 'literal'
	assert decode_native_c_string('one \\n\\t\\r\\a\\b\\f\\v') == 'one \n\t\r\a\b\f\v'
	assert decode_native_c_string('\\\\n') == '\\n'
	assert decode_native_c_string('\\"\\\'\\$') == '"\'$'
	assert decode_native_c_string('\\xFF\\101').bytes() == [u8(255), `A`]
	assert decode_native_c_string('zero\\0tail').bytes() == 'zero\0tail'.bytes()
	assert decode_native_c_string('\\u03bb') == 'λ'
	assert decode_native_c_string('before\\\n  after') == 'beforeafter'
}
