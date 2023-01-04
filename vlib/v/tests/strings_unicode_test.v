fn test_raw_string() {
	assert r'\n\u00c0' == '\\n\\u00c0'
}

fn test_escape() {
	assert '\x20' == ' '
	assert '\u0020' == ' '
	assert '\u00c4' == 'Ä'
	assert '\r\n'.bytes() == [u8(0x0d), 0x0a]
}
