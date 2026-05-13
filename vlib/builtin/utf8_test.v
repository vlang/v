fn test_utf8_char_len() {
	assert utf8_char_len(`a`) == 1
	println(utf8_char_len(`a`))
	s := 'п'
	assert utf8_char_len(s[0]) == 2
}

fn test_utf8_wide_char() {
	$if msvc {
		// TODO: make this test pass msvc too
		return
	}
	r := `✔`
	s := '✔'
	println('r: ${r}')
	println('s: ${s}')
	rstr := r.str()
	println('rstr: ${rstr}')
	assert utf8_char_len(r) == 1
	assert utf8_char_len(s[0]) == 3
	assert s == rstr
	val := rstr.str
	unsafe {
		assert val[0].hex() == 'e2'
		assert val[1].hex() == '9c'
		assert val[2].hex() == '94'
	}
}

fn test_to_wide_latin() {
	s := 'abc 123'
	w := s.to_wide()
	unsafe {
		assert w[0] == 97
		assert w[1] == 98
		assert w[2] == 99
		assert w[3] == 32
		assert w[4] == 49
		assert w[5] == 50
		assert w[6] == 51
		assert w[7] == 0
	}
}

fn test_to_wide_cyrillic() {
	s := 'Проба'
	w := s.to_wide()
	unsafe {
		assert w[0] == 1055
		assert w[1] == 1088
		assert w[2] == 1086
		assert w[3] == 1073
		assert w[4] == 1072
		assert w[5] == 0
	}
}

const little_serial_number = [u8(67), 0, 76, 0, 52, 0, 54, 0, 73, 0, 49, 0, 65, 0, 48, 0, 48, 0,
	54, 0, 52, 0, 57, 0, 0, 0, 0]
const big_serial_number = [u8(0), 67, 0, 76, 0, 52, 0, 54, 0, 73, 0, 49, 0, 65, 0, 48, 0, 48, 0,
	54, 0, 52, 0, 57, 0, 0, 0, 0]

const swide_serial_number = 'CL46I1A00649'

fn test_string_from_wide() {
	$if little_endian {
		z := unsafe { string_from_wide(little_serial_number.data) }
		assert z == swide_serial_number
	} $else {
		z := unsafe { string_from_wide(big_serial_number.data) }
		assert z == swide_serial_number
	}
}

fn test_string_from_wide2() {
	$if little_endian {
		z := unsafe { string_from_wide2(little_serial_number.data, 12) }
		assert z == swide_serial_number
	} $else {
		z := unsafe { string_from_wide2(big_serial_number.data, 12) }
		assert z == swide_serial_number
	}
}

fn test_reverse_cyrillic_with_string_from_wide() {
	s := 'Проба'
	ws := s.to_wide()
	z := unsafe { string_from_wide(ws) }
	assert z == s
}

fn test_wide_to_ansi() {
	ws := 'abc'.to_wide()
	assert wide_to_ansi(ws) == [u8(97), 98, 99, 0]
}

fn test_string_to_ansi_not_null_terminated() {
	assert string_to_ansi_not_null_terminated('abc') == [u8(97), 98, 99]
}

fn test_utf8_str_visible_length() {
	assert utf8_str_visible_length('𝐀𝐁𝐂') == 3
	assert utf8_str_visible_length('\u006E\u0303') == 1
	assert utf8_str_visible_length('\U0001F3F3\uFE0F\u200D\U0001F308') == 2
	assert utf8_str_visible_length('ห์') == 1
	assert utf8_str_visible_length('ปีเตอร์') == 5
	assert utf8_str_visible_length('👩🏽‍💻') == 2
}

fn test_utf8_to_utf32_cases() {
	test_case1 := 'A'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case1.data), test_case1.len) == rune(`A`)

	test_case2 := 'é'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case2.data), test_case2.len) == rune(`é`)

	test_case3 := '€'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case3.data), test_case3.len) == rune(`€`)

	test_case4 := '𐍈'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case4.data), test_case4.len) == rune(0x10348)
	assert impl_utf8_to_utf32(&u8(test_case4.data), test_case4.len) == rune(`𐍈`)

	test_case5 := '中'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case5.data), test_case5.len) == rune(0x4E2D)
	assert impl_utf8_to_utf32(&u8(test_case5.data), test_case5.len) == rune(`中`)

	// emoji, 4-byte UTF-8
	test_case6 := '😀'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case6.data), test_case6.len) == rune(0x1F600)
	assert impl_utf8_to_utf32(&u8(test_case6.data), test_case6.len) == `😀`

	test_case7 := 'Ж'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case7.data), test_case7.len) == rune(`Ж`)

	test_case8 := 'م'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case8.data), test_case8.len) == rune(`م`)

	test_case9 := '߿'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case9.data), test_case9.len) == rune(0x07FF)
	assert impl_utf8_to_utf32(&u8(test_case9.data), test_case9.len) == rune(`߿`)

	test_case10 := 'ࠀ'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case10.data), test_case10.len) == rune(0x0800)
	assert impl_utf8_to_utf32(&u8(test_case10.data), test_case10.len) == rune(`ࠀ`)

	test_case11 := '￿'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case11.data), test_case11.len) == rune(0xFFFF)
	assert impl_utf8_to_utf32(&u8(test_case11.data), test_case11.len) == rune(`￿`)

	test_case12 := '𐀀'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case12.data), test_case12.len) == rune(0x10000)
	assert impl_utf8_to_utf32(&u8(test_case12.data), test_case12.len) == rune(`𐀀`)

	test_case13 := '􏿿'.bytes()
	assert impl_utf8_to_utf32(&u8(test_case13.data), test_case13.len) == rune(0x10FFFF)
	assert impl_utf8_to_utf32(&u8(test_case13.data), test_case13.len) == rune(`􏿿`)
}

fn test_utf8_to_utf32_invalid_length() {
	// More than 4 bytes is invalid
	invalid := [u8(0xF0), 0x9F, 0x98, 0x80, 0x00]
	assert impl_utf8_to_utf32(&u8(invalid.data), invalid.len) == 0
}

fn test_utf8_to_utf32_empty() {
	assert impl_utf8_to_utf32(&u8([]u8{}.data), 0) == 0
}
