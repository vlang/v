module utf16

fn test_encode_bmp() {
	assert encode('') == []u16{}
	assert encode('abc') == [u16(0x61), u16(0x62), u16(0x63)]
	assert encode('€') == [u16(0x20AC)]
	assert encode_rune(`A`) == [u16(0x41)]
	assert encode_rune(0x20AC) == [u16(0x20AC)]
}

fn test_encode_surrogate_pairs() {
	assert encode('😀') == [u16(0xD83D), u16(0xDE00)]
	assert encode('𝄞') == [u16(0xD834), u16(0xDD1E)]
	assert encode_rune(0x1F600) == [u16(0xD83D), u16(0xDE00)]
	// U+10FFFF is the highest valid code point.
	assert encode_rune(0x10FFFF) == [u16(0xDBFF), u16(0xDFFF)]
}

fn test_decode_bmp() {
	assert decode([]u16{}) == ''
	assert decode([u16(0x61), u16(0x62), u16(0x63)]) == 'abc'
	assert decode([u16(0x20AC)]) == '€'
	assert decode([u16(0xFFFD)]) == '\uFFFD'
}

fn test_decode_surrogate_pairs() {
	assert decode([u16(0xD83D), u16(0xDE00)]) == '😀'
	assert decode([u16(0xDBFF), u16(0xDFFF)]) == '\U0010FFFF'
	r, n := decode_rune([u16(0xD83D), u16(0xDE00), u16(0x61)])
	assert r == rune(0x1F600)
	assert n == 2
	r2, n2 := decode_rune([u16(0x61), u16(0x62)])
	assert r2 == rune(0x61)
	assert n2 == 1
}

fn test_round_trip() {
	text := 'V Lang 😀 € π 𝄞'
	assert decode(encode(text)) == text
}

fn test_invalid_sequences_are_replaced() {
	replacement := '\uFFFD'
	// Lone high surrogate.
	assert decode([u16(0xD800)]) == replacement
	// Lone low surrogate.
	assert decode([u16(0xDC00)]) == replacement
	// High surrogate followed by a non-low-surrogate.
	assert decode([u16(0xD800), u16(0x61)]) == '${replacement}a'
	// Truncated pair at the end.
	assert decode([u16(0x61), u16(0xD800)]) == 'a${replacement}'
	// Invalid runes encode as a single replacement unit.
	assert encode_rune(0x110000) == [u16(0xFFFD)]
	assert encode_rune(0xD800) == [u16(0xFFFD)]
}

fn test_decode_strict_rejects_invalid_sequences() {
	decode_strict([u16(0xD800)]) or {
		assert err.msg().contains('unpaired high surrogate')
		return
	}
	assert false
}

fn test_decode_strict_rejects_unpaired_low_surrogate() {
	decode_strict([u16(0xDC00)]) or {
		assert err.msg().contains('unpaired low surrogate')
		return
	}
	assert false
}

fn test_decode_strict_accepts_valid_text() {
	assert decode_strict([u16(0xD83D), u16(0xDE00)])! == '😀'
	assert decode_strict([u16(0x61), u16(0x62)])! == 'ab'
}

fn test_byte_order() {
	assert encode_le('A') == [u8(0x41), 0x00]
	assert encode_be('A') == [u8(0x00), 0x41]
	assert encode_le('€') == [u8(0xAC), 0x20]
	assert encode_be('€') == [u8(0x20), 0xAC]
	assert encode_le('😀') == [u8(0x3D), 0xD8, 0x00, 0xDE]
	assert encode_be('😀') == [u8(0xD8), 0x3D, 0xDE, 0x00]
	assert decode_le([u8(0xAC), 0x20])! == '€'
	assert decode_be([u8(0x20), 0xAC])! == '€'
	assert decode_le([u8(0x3D), 0xD8, 0x00, 0xDE])! == '😀'
	assert decode_be([u8(0xD8), 0x3D, 0xDE, 0x00])! == '😀'
}

fn test_byte_order_rejects_odd_length() {
	decode_le([u8(0x61)]) or {
		assert err.msg().contains('odd byte length')
		return
	}
	assert false
}

fn test_helpers() {
	assert is_surrogate(rune(0xD800))
	assert is_surrogate(rune(0xDFFF))
	assert !is_surrogate(rune(0xD7FF))
	assert !is_surrogate(rune(0xE000))
	assert is_valid_rune(rune(0x10FFFF))
	assert !is_valid_rune(rune(0x110000))
	assert !is_valid_rune(rune(0xD800))
	assert rune_len(rune(0x20AC)) == 1
	assert rune_len(rune(0x1F600)) == 2
}
