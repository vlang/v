import encoding.iconv

fn test_vstring_to_encoding() {
	if empty_utf8 := iconv.vstring_to_encoding('', 'UTF8') {
		assert empty_utf8 == []
	} else {
		panic('${@MOD}.${@FN}: platform does not support UTF8 encoding')
	}

	if abc_utf8 := iconv.vstring_to_encoding('abc', 'UTF8') {
		assert abc_utf8 == [u8(97), 98, 99]
	} else {
		panic('${@MOD}.${@FN}: platform does not support UTF8 encoding')
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
	if empty_utf8 := iconv.encoding_to_vstring([], 'UTF8') {
		assert empty_utf8 == ''
	} else {
		panic('${@MOD}.${@FN}: platform does not support UTF8 encoding')
	}

	if abc_utf8 := iconv.encoding_to_vstring([u8(97), 98, 99], 'UTF8') {
		assert abc_utf8 == 'abc'
	} else {
		panic('${@MOD}.${@FN}: platform does not support UTF8 encoding')
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

fn test_vstring_to_encoding_endian() {
	$if big_endian || macos {
		// The macOS version uses GNU libiconv which always outputs big-endian data no matter what.
		// Ubuntu version is almost certainly the one provided by glibc instead, which uses native endianness.
		// https://unix.stackexchange.com/questions/599582/how-iconv-and-od-handle-endianness
		if abc_utf16 := iconv.vstring_to_encoding('abc', 'UTF16') {
			assert abc_utf16 == [u8(0), 97, 0, 98, 0, 99]
		} else {
			// some platforms do not support UTF16, skip
			assert true
		}

		if abc_utf32 := iconv.vstring_to_encoding('abc', 'UTF32') {
			assert abc_utf32 == [u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99]
		} else {
			// some platforms do not support UTF32, skip
			assert true
		}
	} $else {
		if abc_utf16 := iconv.vstring_to_encoding('abc', 'UTF16') {
			assert abc_utf16 == [u8(97), 0, 98, 0, 99, 0]
		} else {
			// some platforms do not support UTF16, skip
			assert true
		}

		if abc_utf32 := iconv.vstring_to_encoding('abc', 'UTF32') {
			assert abc_utf32 == [u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0]
		} else {
			// some platforms do not support UTF32, such as windows, skip
			assert true
		}
	}
}

fn test_encoding_to_vstring_endian() {
	$if big_endian || macos {
		// The macOS version uses GNU libiconv which always outputs big-endian data no matter what.
		// Ubuntu version is almost certainly the one provided by glibc instead, which uses native endianness.
		// https://unix.stackexchange.com/questions/599582/how-iconv-and-od-handle-endianness
		if abc_utf16 := iconv.encoding_to_vstring([u8(0), 97, 0, 98, 0, 99], 'UTF16') {
			assert abc_utf16 == 'abc'
		} else {
			// some platforms do not support UTF16, skip
			assert true
		}
		if abc_utf32 := iconv.encoding_to_vstring([u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99],
			'UTF32')
		{
			assert abc_utf32 == 'abc'
		} else {
			// some platforms do not support UTF32, skip
			assert true
		}
	} $else {
		if abc_utf16 := iconv.encoding_to_vstring([u8(97), 0, 98, 0, 99, 0], 'UTF16') {
			assert abc_utf16 == 'abc'
		} else {
			// some platforms do not support UTF16, skip
			assert true
		}
		if abc_utf32 := iconv.encoding_to_vstring([u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0],
			'UTF32')
		{
			assert abc_utf32 == 'abc'
		} else {
			// some platforms do not support UTF32, skip
			assert true
		}
	}
}

fn test_create_utf_string_with_bom() {
	assert iconv.create_utf_string_with_bom([u8(97), 98, 99], .utf8) == [u8(0xEF), 0xBB, 0xBF,
		97, 98, 99]
	assert iconv.create_utf_string_with_bom([u8(97), 0, 98, 0, 99, 0], .utf16le) == [
		u8(0xFF),
		0xFE,
		97,
		0,
		98,
		0,
		99,
		0,
	]
	assert iconv.create_utf_string_with_bom([u8(0), 97, 0, 98, 0, 99], .utf16be) == [
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
		.utf32le) == [u8(0xFF), 0xFE, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0]
	assert iconv.create_utf_string_with_bom([u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99],
		.utf32be) == [u8(0), 0, 0xFE, 0xFF, 0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99]
}
