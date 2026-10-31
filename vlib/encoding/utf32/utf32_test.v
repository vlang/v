module utf32

fn test_round_trip() {
	text := 'V Lang 😀 € π'
	assert decode(encode(text)) == text
}

fn test_encode_basic() {
	assert encode('abc') == [u32(0x61), u32(0x62), u32(0x63)]
	assert encode('😀') == [u32(0x1F600)]
}

fn test_decode_invalid() {
	assert decode([u32(0x110000)]) == '\uFFFD'
	assert decode([u32(0xD800)]) == '\uFFFD'
}

fn test_byte_order() {
	assert encode_le('A') == [u8(0x41), 0, 0, 0]
	assert encode_be('A') == [u8(0), 0, 0, 0x41]
	assert decode_le([u8(0x41), 0, 0, 0])! == 'A'
	assert decode_be([u8(0), 0, 0, 0x41])! == 'A'
}

fn test_odd_length() {
	if _ := decode_le([u8(0x41), 0, 0]) {
		assert false
	} else {
		assert err.msg().contains('not a multiple of 4')
	}
}
