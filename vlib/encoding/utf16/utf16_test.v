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

fn test_rune_len_invalid_values_use_one_replacement_unit() {
	for r in [rune(0xD800), 0xDFFF, 0x110000, 0x80000000, 0xFFFFFFFF] {
		assert !is_valid_rune(r)
		assert encode_rune(r) == [u16(0xFFFD)]
		assert rune_len(r) == 1
	}
}

fn test_scalar_boundaries_and_append() {
	assert encode_rune(0) == [u16(0)]
	assert encode_rune(0xD7FF) == [u16(0xD7FF)]
	assert encode_rune(0xE000) == [u16(0xE000)]
	assert encode_rune(0xFFFF) == [u16(0xFFFF)]
	assert encode_rune(0x10000) == [u16(0xD800), 0xDC00]
	for r in [rune(0), 0xD7FF, 0xE000, 0xFFFF] {
		assert is_valid_rune(r)
		assert rune_len(r) == 1
	}
	for r in [rune(0x10000), 0x10FFFF] {
		assert is_valid_rune(r)
		assert rune_len(r) == 2
	}
	mut units := [u16(0x41)]
	for r in [rune(0), 0xFFFF, 0x10000, 0x10FFFF, 0xD800, 0xFFFFFFFF] {
		append_rune(mut units, r)
	}
	assert units == [u16(0x41), 0, 0xFFFF, 0xD800, 0xDC00, 0xDBFF, 0xDFFF, 0xFFFD, 0xFFFD]
}

fn test_decode_rune_empty_and_invalid_consumption() {
	empty_rune, empty_len := decode_rune([]u16{})
	assert empty_rune == rune(0xFFFD)
	assert empty_len == 0
	for units in [[u16(0xD800)], [u16(0xDC00), 0x61], [u16(0xD800), 0x61]] {
		r, n := decode_rune(units)
		assert r == rune(0xFFFD)
		assert n == 1
	}
}

fn test_decode_recovers_adjacent_surrogates() {
	units := [u16(0xDC00), 0xD800, 0xDBFF, 0xDFFF, 0x61, 0xD800]
	assert decode(units) == '\uFFFD\uFFFD\U0010FFFFa\uFFFD'
	assert decode_strict([u16(0), 0xD7FF, 0xE000, 0xFFFF, 0xFFFD])! == '\x00\uD7FF\uE000\uFFFF\uFFFD'
}

fn test_decode_strict_reports_code_unit_index() {
	if _ := decode_strict([u16(0xD800), 0xDC00, 0xD800, 0x61]) {
		assert false
	} else {
		assert err.msg() == 'utf16: high surrogate at index 2 is not followed by a low surrogate'
	}
	if _ := decode_strict([u16(0x61), 0xD800]) {
		assert false
	} else {
		assert err.msg() == 'utf16: unpaired high surrogate at index 1'
	}
	if _ := decode_strict([u16(0xD800), 0xDC00, 0xDFFF]) {
		assert false
	} else {
		assert err.msg() == 'utf16: unpaired low surrogate at index 2'
	}
}

fn test_byte_decoders_replace_surrogates_and_reject_odd_lengths() {
	assert decode_le([u8(0), 0xD8, 0x61, 0])! == '\uFFFDa'
	assert decode_be([u8(0xDC), 0, 0, 0x61])! == '\uFFFDa'
	for bytes in [[u8(0)], [u8(0), 0, 0]] {
		if _ := decode_le(bytes) {
			assert false
		} else {
			assert err.msg().contains('odd byte length')
		}
		if _ := decode_be(bytes) {
			assert false
		} else {
			assert err.msg().contains('odd byte length')
		}
	}
}

fn test_empty_bytes_and_bom_preservation() {
	assert encode_le('') == []u8{}
	assert encode_be('') == []u8{}
	assert decode_le([]u8{})! == ''
	assert decode_be([]u8{})! == ''
	assert decode_strict([]u16{})! == ''
	text := '\uFEFF\x00\U00010000\U0010FFFF'
	le := [u8(0xFF), 0xFE, 0, 0, 0, 0xD8, 0, 0xDC, 0xFF, 0xDB, 0xFF, 0xDF]
	be := [u8(0xFE), 0xFF, 0, 0, 0xD8, 0, 0xDC, 0, 0xDB, 0xFF, 0xDF, 0xFF]
	assert encode_le(text) == le
	assert encode_be(text) == be
	assert decode_le(le)! == text
	assert decode_be(be)! == text
}

fn test_encode_replaces_malformed_utf8() {
	text := [u8(0xFF), 0x61, 0xED, 0xA0, 0x80].bytestr()
	assert encode(text) == [u16(0xFFFD), 0x61, 0xFFFD, 0xFFFD, 0xFFFD]
}
