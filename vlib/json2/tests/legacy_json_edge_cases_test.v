import json2

// Inputs that the removed `json` module accepted, and that json2 has to handle the
// same way for migrated code.

@[json_as_number]
enum Status {
	ok   = 1
	fail = 2
}

enum Plain {
	one = 1
	two = 2
}

fn test_encode_malformed_utf8_with_escape_unicode() {
	// Every invalid or truncated byte becomes U+FFFD instead of slicing past the end.
	assert json2.encode([u8(0xff)].bytestr(), escape_unicode: true) == '"\\ufffd"'
	assert json2.encode([u8(0xe2), 0x82].bytestr(), escape_unicode: true) == '"\\ufffd\\ufffd"'
	assert json2.encode([u8(0xe2), 0x28, 0xa1].bytestr(), escape_unicode: true) == '"\\ufffd(\\ufffd"'
	assert json2.encode([u8(0xf0), 0x9f, 0x98].bytestr(), escape_unicode: true) == '"\\ufffd\\ufffd\\ufffd"'
	assert json2.encode('aé😀', escape_unicode: true) == '"a\\u00e9\\uD83D\\ude00"'
	// Without escaping, the bytes are written as they are.
	assert json2.encode([u8(0xff)].bytestr()) == '"' + [u8(0xff)].bytestr() + '"'
}

fn test_json_as_number_enum_keeps_undeclared_values() {
	assert json2.decode[Status]('2')! == .fail
	undeclared := json2.decode[Status]('99')!
	assert int(undeclared) == 99
	encoded := json2.encode(undeclared)
	assert encoded == '99'
	assert int(json2.decode[Status](encoded)!) == 99
	// A plain enum still has to name a declared value.
	if _ := json2.decode[Plain]('99') {
		assert false
	}
}
