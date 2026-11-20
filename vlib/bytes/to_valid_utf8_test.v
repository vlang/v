module bytes

fn test_to_valid_utf8() {
	assert to_valid_utf8('abc'.bytes(), ''.bytes()) == 'abc'.bytes()
}

fn test_to_valid_utf8_preserves_valid_sequences() {
	mut raw := []u8{}
	raw << [u8(0x63), 0x61, 0x66, 0xc3, 0xa9, 0xff, 0xe9, 0x9b, 0xaa]
	mut want := []u8{}
	want << [u8(0x63), 0x61, 0x66, 0xc3, 0xa9, u8(`?`), 0xe9, 0x9b, 0xaa]
	assert to_valid_utf8(raw, '?'.bytes()) == want
	assert to_valid_utf8('café'.bytes(), '?'.bytes()) == 'café'.bytes()
}
