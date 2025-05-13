import encoding.utf8

fn test_utf8_util() {
	// string test
	src := 'ăĂ ôÔ testo æ€”' //_\u1E5A\u1E5B<=>\u1F49\u1F41<=>\u0128\u012a\u012c" // len 29 runes, raw 49 bytes
	src_upper := 'ĂĂ ÔÔ TESTO Æ€”' //_\u1E5A\u1E5A<=>\u1F49\u1F49<=>\u0128\u012A\u012C"
	src_lower := 'ăă ôô testo æ€”' //_\u1E5B\u1E5B<=>\u1F41\u1F41<=>\u0129\u012B\u012D"
	upper := utf8.to_upper(src)
	lower := utf8.to_lower(src)
	assert upper == src_upper
	assert lower == src_lower

	assert utf8.to_upper('абвёabc12｛') == 'АБВЁABC12｛'
	assert utf8.to_lower('АБВЁABC12｛') == 'абвёabc12｛'

	// test len function
	assert utf8.len('') == 0
	assert utf8.len('pippo') == 5
	assert utf8.len(src) == 15 // 29
	assert src.len == 24 // 49

	// western punctuation
	a := '.abc?abcòàè.'
	assert utf8.is_punct(a, 0) == true
	assert utf8.is_punct('b', 0) == false
	assert utf8.is_rune_punct(0x002E) == true
	assert utf8.is_punct(a, 4) == true // ?
	assert utf8.is_punct(a, 14) == true // last .
	assert utf8.is_punct(a, 12) == false // è
	println('OK western')

	// global punctuation
	b := '.ĂĂa. ÔÔ TESTO Æ€'
	assert utf8.is_global_punct(b, 0) == true
	assert utf8.is_global_punct('.', 0) == true
	assert utf8.is_rune_punct(0x002E) == true
	assert utf8.is_global_punct(b, 6) == true // .
	assert utf8.is_global_punct(b, 1) == false // a

	// test utility functions
	assert utf8.get_rune(b, 0) == 0x002E
	c := 'a©★🚀'
	assert utf8.get_rune(c, 0) == `a` // 1 byte
	assert utf8.get_rune(c, 1) == `©` // 2 bytes
	assert utf8.get_rune(c, 3) == `★` // 3 bytes
	assert utf8.get_rune(c, 6) == `🚀` // 4 bytes
}

fn test_raw_indexing() {
	a := '我是V Lang!'

	// test non ascii characters
	assert utf8.raw_index(a, 0) == '我'
	assert utf8.raw_index(a, 1) == '是'

	// test ascii characters
	assert utf8.raw_index(a, 2) == 'V'
	assert utf8.raw_index(a, 3) == ' '
	assert utf8.raw_index(a, 4) == 'L'
	assert utf8.raw_index(a, 5) == 'a'
	assert utf8.raw_index(a, 6) == 'n'
	assert utf8.raw_index(a, 7) == 'g'
	assert utf8.raw_index(a, 8) == '!'

	// test differnt utf8 byte lengths
	c := 'a©★🚀'
	assert utf8.raw_index(c, 0) == 'a' // 1 byte
	assert utf8.raw_index(c, 1) == '©' // 2 bytes
	assert utf8.raw_index(c, 2) == '★' // 3 bytes
	assert utf8.raw_index(c, 3) == '🚀' // 4 bytes
}

fn test_reversed() {
	a := '我是V Lang!'
	b := '你好世界hello world'
	assert utf8.reverse(a) == '!gnaL V是我'
	assert utf8.reverse(b) == 'dlrow olleh界世好你'
}

fn test_is_control() {
	for ra in `a` .. `z` {
		assert utf8.is_control(ra) == false
	}

	for ra in `A` .. `Z` {
		assert utf8.is_control(ra) == false
	}

	assert utf8.is_control('\x01'.runes()[0]) == true
	assert utf8.is_control('\u0100'.runes()[0]) == false
}

fn test_is_letter() {
	for ra in `a` .. `z` {
		assert utf8.is_letter(ra) == true
	}

	for ra in `A` .. `Z` {
		assert utf8.is_letter(ra) == true
	}

	assert utf8.is_letter(`ɀ`) == true
	assert utf8.is_letter(`ȶ`) == true
	assert utf8.is_letter(`ȹ`) == true
}

fn test_is_space() {
	for ra in `a` .. `z` {
		assert utf8.is_space(ra) == false
	}

	for ra in `A` .. `Z` {
		assert utf8.is_space(ra) == false
	}

	assert utf8.is_space(`\u202f`) == true
	assert utf8.is_space(`\u2009`) == true
	assert utf8.is_space(`\u00A0`) == true
}

fn test_is_number() {
	for ra in `a` .. `z` {
		assert utf8.is_number(ra) == false
	}

	for ra in `A` .. `Z` {
		assert utf8.is_number(ra) == false
	}

	for ra in `0` .. `1` {
		assert utf8.is_number(ra) == true
	}

	assert utf8.is_number(`\u2164`) == true
	assert utf8.is_number(`\u2188`) == true
	assert utf8.is_number(`\u3029`) == true
}
