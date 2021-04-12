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

	// ustring test
	src1 := src.ustring()
	upper1 := utf8.u_to_upper(src1)
	lower1 := utf8.u_to_lower(src1)
	assert upper1 == (src_upper.ustring())
	assert lower1 == (src_lower.ustring())

	// test len function
	assert utf8.len('') == 0
	assert utf8.len('pippo') == 5
	assert utf8.len(src) == 15 // 29
	assert src.len == 24 // 49
	// test u_len function
	assert utf8.u_len(''.ustring()) == 0
	assert utf8.u_len(src1) == 15 // 29
	assert utf8.u_len('pippo'.ustring()) == 5

	// western punctuation
	a := '.abc?abcòàè.'
	assert utf8.is_punct(a, 0) == true
	assert utf8.is_punct('b', 0) == false
	assert utf8.is_uchar_punct(0x002E) == true
	assert utf8.is_punct(a, 4) == true // ?
	assert utf8.is_punct(a, 14) == true // last .
	assert utf8.is_punct(a, 12) == false // è
	println('OK western')

	// global punctuation
	b := '.ĂĂa. ÔÔ TESTO Æ€'
	assert utf8.is_global_punct(b, 0) == true
	assert utf8.is_global_punct('.', 0) == true
	assert utf8.is_uchar_punct(0x002E) == true
	assert utf8.is_global_punct(b, 6) == true // .
	assert utf8.is_global_punct(b, 1) == false // a

	// test utility functions
	assert utf8.get_uchar(b, 0) == 0x002E
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
}
