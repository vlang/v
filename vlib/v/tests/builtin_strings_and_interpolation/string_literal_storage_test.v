// vtest vflags: -nocache

const literal_storage_text = 'a\x00b'

struct LiteralStorage {
	text string = 'quote " slash \\ newline\n'
}

fn literal_storage_value() string {
	return 'a\x00b'
}

fn test_string_literal_storage_preserves_bytes_and_lifetime() {
	text := literal_storage_value()
	assert text.len == 3
	assert text.bytes() == [u8(`a`), 0, `b`]
	assert text == literal_storage_text
	// The C terminator lies just beyond V's string bounds.
	assert unsafe { text.str[3] } == 0
	// Manual free must recognize the literal flag and preserve its static bytes.
	unsafe { text.free() }
	assert text == literal_storage_text
	assert ''.len == 0
	assert 'hé🙂'.len == 7
	escaped := LiteralStorage{}.text
	assert escaped.len == 24
	assert escaped[6] == 34
	assert escaped[14] == 92
	assert escaped[23] == 10
}

fn test_string_literals_work_in_arrays_and_interpolation() {
	values := ['a\x00b', 'hé🙂', '']!
	assert values[0] == literal_storage_text
	assert values[1].bytes() == [u8(104), 195, 169, 240, 159, 153, 130]
	assert values[2] == ''
	assert '(${values[0]})'.bytes() == [u8(`(`), `a`, 0, `b`, `)`]
}
