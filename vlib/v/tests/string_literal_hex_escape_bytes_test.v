fn test_string_literal_bytes_keeps_escaped_backslash_byte() {
	expected := [u8(0x59), 0x3c, 0x5c, 0x52, 0x18]
	actual := '\x59\x3c\x5c\x52\x18'.bytes()
	assert actual == expected
}
