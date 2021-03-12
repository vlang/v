fn test_cstring() {
	w := c'world'
	hlen := unsafe{ C.strlen(c'hello') }
	hlen2 := unsafe{ C.strlen('hello') }
	wlen := unsafe{ C.strlen(w) }
	assert hlen == 5
	assert hlen2 == 5
	assert wlen == 5
}

fn test_cstring_with_zeros() {
	rawbytes := c'\x00username\x00password'
	s := unsafe { rawbytes.vstring_with_len(18) }
	h := s.bytes().hex()
	assert h == '00757365726e616d650070617373776f7264'
}

fn test_raw_string() {
	assert r'\n\u00c0' == '\\n\\u00c0'
}

fn test_escape() {
	assert '\x20' == ' '
	assert '\u0020' == ' '
	assert '\u00c4'.bytes() == 'Ä'.bytes()
	assert '\u00c4' == 'Ä'
	assert '\u00C4' == 'Ä'
	assert '\r\n'.bytes() == [byte(0x0d), 0x0a]
}
