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

fn test_get_rune_invalid_utf8() {
	replacement := rune(0xfffd)
	invalid := [u8(0xc1), 0xa1, `-`, 0xed, 0xa0, 0x80, `-`, 0xc3].bytestr()
	assert utf8.get_rune(invalid, 0) == replacement
	assert utf8.get_rune(invalid, 1) == replacement
	assert utf8.get_rune(invalid, 2) == `-`
	assert utf8.get_rune(invalid, 3) == replacement
	assert utf8.get_rune(invalid, 4) == replacement
	assert utf8.get_rune(invalid, 5) == replacement
	assert utf8.get_rune(invalid, 6) == `-`
	assert utf8.get_rune(invalid, 7) == replacement
}

fn test_get_rune_out_of_range_is_not_nul() {
	replacement := rune(0xfffd)
	assert utf8.get_rune('', 0) == replacement
	assert utf8.get_rune('', 1) == replacement
	assert utf8.get_rune('a', -1) == replacement
	assert utf8.get_rune('a', 1) == replacement // index == len
	assert utf8.get_rune('a', 999) == replacement
	assert utf8.get_rune('a★', 4) == replacement // index == len
	// a NUL that is in the string is still a NUL
	assert utf8.get_rune('\x00', 0) == rune(0)
	assert utf8.get_rune('a\x00b', 1) == rune(0)
	assert utf8.get_rune('\x00', 1) == replacement
	// the predicates that take an index see no punctuation outside of the string
	assert utf8.is_punct('', 0) == false
	assert utf8.is_punct('.', 1) == false
	assert utf8.is_global_punct('', 0) == false
	assert utf8.is_global_punct('.', 1) == false
}

fn check_decode_rune_at(s string, index int, expected_rune rune, expected_size int) {
	r, size := utf8.decode_rune_at(s, index)
	assert r == expected_rune, 'rune at index ${index} of ${s.bytes().hex()}'
	assert size == expected_size, 'size at index ${index} of ${s.bytes().hex()}'
	assert utf8.get_rune(s, index) == r
}

fn test_decode_rune_at() {
	replacement := rune(0xfffd)
	// valid sequences return the rune and its length in bytes
	c := 'a©★🚀'
	check_decode_rune_at(c, 0, `a`, 1)
	check_decode_rune_at(c, 1, `©`, 2)
	check_decode_rune_at(c, 3, `★`, 3)
	check_decode_rune_at(c, 6, `🚀`, 4)
	check_decode_rune_at('\x00', 0, rune(0), 1)
	// outside of the string: the replacement rune with a size of 0
	check_decode_rune_at('', 0, replacement, 0)
	check_decode_rune_at('', -1, replacement, 0)
	check_decode_rune_at('a', -1, replacement, 0)
	check_decode_rune_at('a', 1, replacement, 0)
	check_decode_rune_at('a', 999, replacement, 0)
	check_decode_rune_at(c, c.len, replacement, 0)
	check_decode_rune_at('\x00', 1, replacement, 0)
	// invalid sequences: the replacement rune with a size of 1
	check_decode_rune_at(c, 2, replacement, 1) // second byte of ©
	check_decode_rune_at(c, 4, replacement, 1) // second byte of ★
	check_decode_rune_at(c, 9, replacement, 1) // last byte of 🚀
	check_decode_rune_at([u8(0xff)].bytestr(), 0, replacement, 1)
	check_decode_rune_at([u8(0xf5), `a`].bytestr(), 0, replacement, 1)
	check_decode_rune_at([u8(0xe2), 0x98].bytestr(), 0, replacement, 1) // truncated ★
	check_decode_rune_at([u8(0xc1), 0xa1].bytestr(), 0, replacement, 1) // overlong
	check_decode_rune_at([u8(0xed), 0xa0, 0x80].bytestr(), 0, replacement, 1) // surrogate
	// a replacement rune that is really in the string has its own size of 3
	check_decode_rune_at('�', 0, replacement, 3)
	check_decode_rune_at('a�', 1, replacement, 3)
}

fn test_decode_rune_at_walks_a_string() {
	s := 'a©' + [u8(0xff)].bytestr() + '★\x00🚀'
	mut runes := []rune{}
	mut sizes := []int{}
	mut i := 0
	for {
		r, size := utf8.decode_rune_at(s, i)
		if size == 0 {
			break
		}
		runes << r
		sizes << size
		i += size
	}
	assert i == s.len
	assert runes == [`a`, `©`, rune(0xfffd), `★`, rune(0), `🚀`]
	assert sizes == [1, 2, 1, 3, 1, 4]
	assert runes.len == utf8.len(s)
}

fn test_invalid_utf8_indexing_advances_one_byte() {
	replacement := rune(0xfffd).str()
	invalid := [u8(0xf5), `a`].bytestr()
	assert utf8.get_rune(invalid, 0) == rune(0xfffd)
	assert utf8.get_rune(invalid, 1) == `a`
	assert utf8.len(invalid) == 2
	assert utf8.raw_index(invalid, 0) == replacement
	assert utf8.raw_index(invalid, 1) == 'a'
	assert utf8.reverse(invalid) == 'a' + replacement
	assert utf8.to_upper(invalid) == [u8(0xf5), `A`].bytestr()
	assert utf8.to_lower([u8(0xf5), `A`].bytestr()) == invalid
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
