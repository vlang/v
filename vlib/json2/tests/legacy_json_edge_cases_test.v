import json2

// Inputs that the removed `json` module accepted, and that json2 has to handle the
// same way for migrated code.

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
