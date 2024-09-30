import encoding.iconv

fn test_vstring_to_encoding() {
	assert iconv.vstring_to_encoding('abc', 'UTF8') == [u8(97), 98, 99]
	$if linux {
		assert iconv.vstring_to_encoding('abc', 'UTF16') == [u8(255), 254, 97, 0, 98, 0, 99, 0]
	}
	$if windows {
		assert iconv.vstring_to_encoding('abc', 'UTF16') == [u8(97), 0, 98, 0, 99, 0]
	}
	$if !macos {
		// CI macos fail, maybe not support UTF16LE
		assert iconv.vstring_to_encoding('abc', 'UTF16LE') == [u8(97), 0, 98, 0, 99, 0]
	}
	// CI ubuntu-docker-musl fail with this
	// assert iconv.vstring_to_encoding('V大法好abc', 'GB2312') == [u8(86), 180, 243, 183, 168,
	//	186, 195, 97, 98, 99]
	assert iconv.vstring_to_encoding('abc', 'encoding_not_exist') == []
	assert iconv.vstring_to_encoding('', 'UTF16LE') == []
}

fn test_encoding_to_vstring() {
	assert iconv.encoding_to_vstring([u8(97), 98, 99], 'UTF8') == 'abc'
	$if !macos {
		// CI macos fail, maybe not support UTF16LE
		assert iconv.encoding_to_vstring([u8(97), 0, 98, 0, 99, 0], 'UTF16LE') == 'abc'
	}
	// CI ubuntu-docker-musl fail with this
	// assert iconv.encoding_to_vstring([u8(86), 180, 243, 183, 168, 186, 195, 97, 98, 99],
	//	'GB2312') == 'V大法好abc'
	assert iconv.encoding_to_vstring([u8(97), 98, 99], 'encoding_not_exist') == ''
	assert iconv.encoding_to_vstring([], 'UTF16LE') == ''
}
