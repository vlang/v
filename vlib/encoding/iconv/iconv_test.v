import encoding.iconv

fn test_vstring_to_encoding() {
	empty_utf8 := iconv.vstring_to_encoding('', 'UTF8')!
	assert empty_utf8 == []

	abc_utf8 := iconv.vstring_to_encoding('abc', 'UTF8')!
	assert abc_utf8 == [u8(97), 98, 99]

	abc_utf16le := iconv.vstring_to_encoding('abc', 'UTF16LE')!
	assert abc_utf16le == [u8(97), 0, 98, 0, 99, 0]

	abc_utf16be := iconv.vstring_to_encoding('abc', 'UTF16BE')!
	assert abc_utf16be == [u8(0), 97, 0, 98, 0, 99]

	abc_utf32le := iconv.vstring_to_encoding('abc', 'UTF32LE')!
	assert abc_utf32le == [u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0]

	abc_utf32be := iconv.vstring_to_encoding('abc', 'UTF32BE')!
	assert abc_utf32be == [u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99]

	if abc_not_exist := iconv.vstring_to_encoding('abc', 'encoding_not_exist') {
		assert false, 'encoding_not_exist'
	}

	if ch_str := iconv.vstring_to_encoding('V大法好abc', 'GB2312') {
		assert ch_str == [u8(86), 180, 243, 183, 168, 186, 195, 97, 98, 99]
	} else {
		// some platforms do not support GB2312, skip
		assert true
	}
}

fn test_encoding_to_vstring() {
	empty_utf8 := iconv.encoding_to_vstring([], 'UTF8')!
	assert empty_utf8 == ''

	abc_utf8 := iconv.encoding_to_vstring([u8(97), 98, 99], 'UTF8')!
	assert abc_utf8 == 'abc'

	abc_utf16le := iconv.encoding_to_vstring([u8(97), 0, 98, 0, 99, 0], 'UTF16LE')!
	assert abc_utf16le == 'abc'

	abc_utf16be := iconv.encoding_to_vstring([u8(0), 97, 0, 98, 0, 99], 'UTF16BE')!
	assert abc_utf16be == 'abc'

	abc_utf32le := iconv.encoding_to_vstring([u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0],
		'UTF32LE')!
	assert abc_utf32le == 'abc'

	abc_utf32be := iconv.encoding_to_vstring([u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99],
		'UTF32BE')!
	assert abc_utf32be == 'abc'

	if abc_not_exist := iconv.encoding_to_vstring([u8(97), 98, 99], 'encoding_not_exist') {
		assert false, 'encoding_not_exist'
	}

	if ch_str := iconv.encoding_to_vstring([u8(86), 180, 243, 183, 168, 186, 195, 97, 98, 99],
		'GB2312')
	{
		assert ch_str == 'V大法好abc'
	} else {
		// some platforms do not support GB2312, skip
		assert true
	}
}

fn test_create_utf_string_with_bom() {
	assert iconv.create_utf_string_with_bom([u8(97), 98, 99], 'UTF8') == [u8(0xEF), 0xBB, 0xBF,
		97, 98, 99]
	assert iconv.create_utf_string_with_bom([u8(97), 0, 98, 0, 99, 0], 'UTF16LE') == [
		u8(0xFF),
		0xFE,
		97,
		0,
		98,
		0,
		99,
		0,
	]
	assert iconv.create_utf_string_with_bom([u8(0), 97, 0, 98, 0, 99], 'UTF16BE') == [
		u8(0xFE),
		0xFF,
		0,
		97,
		0,
		98,
		0,
		99,
	]
	assert iconv.create_utf_string_with_bom([u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0],
		'UTF32LE') == [u8(0xFF), 0xFE, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0]
	assert iconv.create_utf_string_with_bom([u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99],
		'UTF32BE') == [u8(0), 0, 0xFE, 0xFF, 0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99]
}

fn test_remove_utf_string_with_bom() {
	assert iconv.remove_utf_string_with_bom([u8(0xEF), 0xBB, 0xBF, 97, 98, 99], 'UTF8') == [
		u8(97),
		98,
		99,
	]
	assert iconv.remove_utf_string_with_bom([u8(0xFF), 0xFE, 97, 0, 98, 0, 99, 0], 'UTF16LE') == [
		u8(97),
		0,
		98,
		0,
		99,
		0,
	]
	assert iconv.remove_utf_string_with_bom([u8(0xFE), 0xFF, 0, 97, 0, 98, 0, 99], 'UTF16BE') == [
		u8(0),
		97,
		0,
		98,
		0,
		99,
	]
	assert iconv.remove_utf_string_with_bom([u8(0xFF), 0xFE, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99,
		0, 0, 0], 'UTF32LE') == [u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0]
	assert iconv.remove_utf_string_with_bom([u8(0), 0, 0xFE, 0xFF, 0, 0, 0, 97, 0, 0, 0, 98, 0,
		0, 0, 99], 'UTF32BE') == [u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99]
}
