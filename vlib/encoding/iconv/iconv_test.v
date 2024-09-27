import encoding.iconv

fn test_vstring_to_encoding() {
	assert iconv.vstring_to_encoding('UTF8', 'abc') == [u8(97), 98, 99]
	assert iconv.vstring_to_encoding('GB18030', 'V大法好') == [u8(86), 180, 243, 183, 168, 186,
		195]
	assert iconv.vstring_to_encoding('encoding_not_exist', 'abc') == []
	assert iconv.vstring_to_encoding('GB18030', '') == []
}

fn test_encoding_to_vstring() {
	assert iconv.encoding_to_vstring('UTF8', [u8(97), 98, 99]) == 'abc'
	assert iconv.encoding_to_vstring('GB18030', [u8(86), 180, 243, 183, 168, 186, 195]) == 'V大法好'
	assert iconv.encoding_to_vstring('encoding_not_exist', [u8(97), 98, 99]) == ''
	assert iconv.encoding_to_vstring('GB18030', []) == ''
}
