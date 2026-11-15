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

fn test_empty_input() {
	assert encode('') == []u32{}
	assert decode([]u32{}) == ''
	assert encode_le('') == []u8{}
	assert encode_be('') == []u8{}
	assert decode_le([]u8{})! == ''
	assert decode_be([]u8{})! == ''
}

fn test_scalar_boundaries() {
	text := '\x00\uD7FF\uE000\uFFFF\U00010000\U0010FFFF'
	units := [u32(0), 0xD7FF, 0xE000, 0xFFFF, 0x10000, 0x10FFFF]
	assert encode(text) == units
	assert decode(units) == text
}

fn test_invalid_units_preserve_surrounding_text() {
	units := [u32(0x41), 0xD800, 0xDFFF, 0x110000, 0x80000000, 0xFFFFFFFF, 0x42]
	assert decode(units) == 'A\uFFFD\uFFFD\uFFFD\uFFFD\uFFFDB'
}

fn test_multibyte_order_and_bom_preservation() {
	text := '\uFEFF\x00\u20AC\U00010000\U0010FFFF'
	le := [u8(0xFF), 0xFE, 0, 0, 0, 0, 0, 0, 0xAC, 0x20, 0, 0, 0, 0, 1, 0, 0xFF, 0xFF, 0x10, 0]
	be := [u8(0), 0, 0xFE, 0xFF, 0, 0, 0, 0, 0, 0, 0x20, 0xAC, 0, 1, 0, 0, 0, 0x10, 0xFF, 0xFF]
	assert encode_le(text) == le
	assert encode_be(text) == be
	assert decode_le(le)! == text
	assert decode_be(be)! == text
}

fn test_byte_decoders_replace_invalid_units() {
	assert decode_le([u8(0), 0xD8, 0, 0, 0, 0, 0x11, 0, 0xFF, 0xFF, 0xFF, 0xFF])! == '\uFFFD\uFFFD\uFFFD'
	assert decode_be([u8(0), 0, 0xDF, 0xFF, 0x80, 0, 0, 0, 0xFF, 0xFF, 0xFF, 0xFF])! == '\uFFFD\uFFFD\uFFFD'
}

fn test_byte_decoders_reject_every_incomplete_unit_length() {
	for n in [1, 2, 3, 5, 6, 7] {
		bytes := []u8{len: n}
		if _ := decode_le(bytes) {
			assert false
		} else {
			assert err.msg() == 'utf32: byte length ${n} is not a multiple of 4'
		}
		if _ := decode_be(bytes) {
			assert false
		} else {
			assert err.msg() == 'utf32: byte length ${n} is not a multiple of 4'
		}
	}
}

fn test_encode_replaces_malformed_utf8() {
	text := [u8(0xFF), 0x61, 0xED, 0xA0, 0x80].bytestr()
	assert encode(text) == [u32(0xFFFD), 0x61, 0xFFFD, 0xFFFD, 0xFFFD]
	assert decode_le(encode_le(text))! == '\uFFFDa\uFFFD\uFFFD\uFFFD'
	assert decode_be(encode_be(text))! == '\uFFFDa\uFFFD\uFFFD\uFFFD'
}
