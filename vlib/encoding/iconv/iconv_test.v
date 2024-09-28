import encoding.iconv

fn test_vstring_to_encoding() {
	assert iconv.vstring_to_encoding('abc', 'UTF8') == [u8(97), 98, 99]
	assert iconv.vstring_to_encoding('V大法好', 'GB18030') == [u8(86), 180, 243, 183, 168, 186,
		195]
	assert iconv.vstring_to_encoding('abc', 'encoding_not_exist') == []
	assert iconv.vstring_to_encoding('', 'GB18030') == []
}

fn test_encoding_to_vstring() {
	assert iconv.encoding_to_vstring([u8(97), 98, 99], 'UTF8') == 'abc'
	assert iconv.encoding_to_vstring([u8(86), 180, 243, 183, 168, 186, 195], 'GB18030') == 'V大法好'
	assert iconv.encoding_to_vstring([u8(97), 98, 99], 'encoding_not_exist') == ''
	assert iconv.encoding_to_vstring([], 'GB18030') == ''
}
