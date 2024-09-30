import encoding.iconv

fn test_vstring_to_encoding() {
	if abc_utf8 := iconv.vstring_to_encoding('abc', 'UTF8') {
		assert abc_utf8 == [u8(97), 98, 99]
	} else {
		panic('${@MOD}.${@FN}: platform does not support UTF8 encoding')
	}

	if abc_utf16 := iconv.vstring_to_encoding('abc', 'UTF16') {
		assert abc_utf16 == [u8(97), 0, 98, 0, 99, 0]
	} else {
		panic('${@MOD}.${@FN}: platform does not support UTF16 encoding')
	}

	if abc_utf32 := iconv.vstring_to_encoding('abc', 'UTF32') {
		assert abc_utf32 == [u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0]
	} else {
		// some platforms do not support UTF32, such as windows, skip
		assert true
	}

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
	if abc_utf8 := iconv.encoding_to_vstring([u8(97), 98, 99], 'UTF8') {
		assert abc_utf8 == 'abc'
	} else {
		panic('${@MOD}.${@FN}: platform does not support UTF8 encoding')
	}

	if abc_utf16 := iconv.encoding_to_vstring([u8(97), 0, 98, 0, 99, 0], 'UTF16') {
		assert abc_utf16 == 'abc'
	} else {
		panic('${@MOD}.${@FN}: platform does not support UTF16 encoding')
	}

	if abc_utf32 := iconv.encoding_to_vstring([u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0],
		'UTF32')
	{
		assert abc_utf32 == 'abc'
	} else {
		// some platforms do not support UTF32, skip
		assert true
	}

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
