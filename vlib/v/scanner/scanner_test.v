module scanner

import encoding.utf8
import v.pref
import v.token

fn test_power_tokens() {
	source := 'a ** b **= c'
	mut files := token.FileSet.new()
	mut file := files.add_file('power.v', source.len)
	file.index_lines(source)
	preferences := &pref.Preferences{}
	mut scanner := new_scanner(preferences, .normal)
	scanner.init(file, source)
	assert scanner.scan() == .name
	assert scanner.scan() == .power
	assert scanner.scan() == .name
	assert scanner.scan() == .power_assign
	assert scanner.scan() == .name
	assert scanner.scan() == .semicolon
	assert scanner.scan() == .eof
}

fn test_immediate_invalid_radix_digits() {
	cases := {
		'0b2': 'this binary number has unsuitable digit `2`'
		'0o8': 'this octal number has unsuitable digit `8`'
		'0xG': 'this hexadecimal number has unsuitable digit `G`'
	}
	for source, expected_message in cases {
		mut files := token.FileSet.new()
		mut file := files.add_file('invalid_radix.v', source.len)
		file.index_lines(source)
		preferences := &pref.Preferences{}
		mut scanner := new_scanner(preferences, .normal)
		scanner.init(file, source)

		assert scanner.scan() == .number
		assert scanner.lit == source
		assert scanner.offset == source.len
		assert scanner.diagnostics.len == 1
		assert scanner.diagnostics[0].offset == 2
		assert scanner.diagnostics[0].message == expected_message
	}
}

fn test_all_number_prefixed_identifiers_are_reported() {
	source := '3a := 1\n4b := 2\nprintln(3a)'
	mut files := token.FileSet.new()
	mut file := files.add_file('number_prefixed_identifiers.v', source.len)
	file.index_lines(source)
	preferences := &pref.Preferences{}
	mut scanner := new_scanner(preferences, .normal)
	scanner.init(file, source)

	for scanner.scan() != .eof {
	}
	assert scanner.diagnostics.len == 3
	assert scanner.diagnostics[0].message == 'identifier name `3a` cannot start with a number'
	assert scanner.diagnostics[1].message == 'identifier name `4b` cannot start with a number'
	assert scanner.diagnostics[2].message == 'identifier name `3a` cannot start with a number'
	assert scanner.diagnostics[0].offset == 0
	assert scanner.diagnostics[0].end == 2
	assert scanner.diagnostics[1].offset == 8
	assert scanner.diagnostics[1].end == 10
	assert scanner.diagnostics[2].offset == 24
	assert scanner.diagnostics[2].end == 26
}

fn test_malformed_exponent_suffixes_are_unsuitable_digits() {
	cases := {
		'2Ea':    'this number has unsuitable digit `a`'
		'2e+foo': 'this number has unsuitable digit `f`'
	}
	for source, expected_message in cases {
		mut files := token.FileSet.new()
		mut file := files.add_file('malformed_exponent.v', source.len)
		file.index_lines(source)
		preferences := &pref.Preferences{}
		mut scanner := new_scanner(preferences, .normal)
		scanner.init(file, source)

		assert scanner.scan() == .number
		assert scanner.lit == source
		assert scanner.offset == source.len
		assert scanner.diagnostics.len == 1
		assert scanner.diagnostics[0].message == expected_message
		assert scanner.diagnostics[0].offset == if source == '2Ea' {
			2
		} else {
			3
		}
	}

	mut files := token.FileSet.new()
	mut file := files.add_file('missing_exponent.v', 2)
	file.index_lines('2E')
	preferences := &pref.Preferences{}
	mut scanner := new_scanner(preferences, .normal)
	scanner.init(file, '2E')
	assert scanner.scan() == .number
	assert scanner.diagnostics.len == 1
	assert scanner.diagnostics[0].message == 'exponent has no digits'
	assert scanner.diagnostics[0].offset == 1
}

fn test_invalid_unicode_scalar_escapes_are_reported() {
	cases := {
		r"'\uD8FF'":     r'invalid unicode point `\uD8FF`'
		r"'\U0000D8FF'": r'invalid unicode point `\U0000D8FF`'
		r"'\U00110000'": r'invalid unicode point `\U00110000`'
	}
	for source, expected_message in cases {
		mut files := token.FileSet.new()
		mut file := files.add_file('invalid_unicode.v', source.len)
		file.index_lines(source)
		preferences := &pref.Preferences{}
		mut scanner := new_scanner(preferences, .normal)
		scanner.init(file, source)

		for scanner.scan() != .eof {
		}
		assert scanner.diagnostics.len == 1
		assert scanner.diagnostics[0].message == expected_message
		assert scanner.diagnostics[0].offset == source.len - 1
	}
}

fn test_unknown_string_escape_is_reported() {
	source := r"'\_'"
	mut files := token.FileSet.new()
	mut file := files.add_file('unknown_escape.v', source.len)
	file.index_lines(source)
	preferences := &pref.Preferences{}
	mut scanner := new_scanner(preferences, .normal)
	scanner.init(file, source)

	assert scanner.scan() == .string
	assert scanner.diagnostics.len == 1
	assert scanner.diagnostics[0].message == '`_` unknown escape sequence'
	assert scanner.diagnostics[0].offset == 2
}

fn test_js_prefixed_string_is_one_token() {
	source := "js'hello V'"
	mut files := token.FileSet.new()
	mut file := files.add_file('js_string.js.v', source.len)
	file.index_lines(source)
	preferences := &pref.Preferences{}
	mut scanner := new_scanner(preferences, .normal)
	scanner.init(file, source)

	assert scanner.scan() == .string
	assert scanner.lit == source
	assert scanner.scan() == .semicolon
	assert scanner.scan() == .eof
	assert scanner.diagnostics.len == 0
}

fn test_keyword_enum_selector_inserts_semicolon() {
	source := 'assert value == .fn\nassert value == .struct'
	mut files := token.FileSet.new()
	mut file := files.add_file('keyword_enum_selector.v', source.len)
	file.index_lines(source)
	preferences := &pref.Preferences{}
	mut scanner := new_scanner(preferences, .normal)
	scanner.init(file, source)

	for expected in [token.Token.key_assert, .name, .eq, .dot, .key_fn, .semicolon, .key_assert,
		.name, .eq, .dot, .key_struct, .semicolon, .eof] {
		assert scanner.scan() == expected
	}
}

fn char_literal_diagnostics(source string) []string {
	mut files := token.FileSet.new()
	mut file := files.add_file('char_literal.v', source.len)
	file.index_lines(source)
	preferences := &pref.Preferences{}
	mut scanner := new_scanner(preferences, .normal)
	scanner.init(file, source)
	for scanner.scan() != .eof {
	}
	return scanner.diagnostics.map(it.message)
}

// A character literal holds one character however it is spelled: a three-digit octal
// escape is one byte, and the byte escapes of one UTF-8 sequence are one character, the
// same as in a string.
fn test_char_literal_escapes_that_spell_one_character_are_accepted() {
	for source in [
		r'`\141`',
		r'`\x61`',
		r'`\u0061`',
		r'`\U0001F680`',
		r'`\0`',
		r'`\xc3\xa9`',
		r'`\xe2\x98\x85`',
		r'`\342\230\205`',
		r'`\342\x98\205`',
		r'`\xf0\x9f\x9a\x80`',
	] {
		assert char_literal_diagnostics(source) == [], source
	}
}

fn test_char_literal_with_more_than_one_character_is_still_rejected() {
	for source in [
		r'`\141b`',
		r'`\x61\x62`',
		// A lead byte whose continuation bytes are missing, invalid, or overlong is not
		// one character.
		r'`\xe2\x98`',
		r'`\xe2\x98\x41`',
		r'`\xc0\x80`',
		r'`\xc3\xa9\xa9`',
	] {
		diagnostics := char_literal_diagnostics(source)
		assert diagnostics.len == 1, source
		assert diagnostics[0].ends_with('(more than one character)'), '${source}: ${diagnostics[0]}'
	}
}

fn utf8_sequence_len(lead u8) int {
	return if lead >= 0xf0 {
		4
	} else if lead >= 0xe0 {
		3
	} else {
		2
	}
}

// Byte escapes are one character exactly when they spell one well-formed UTF-8 sequence,
// which is decided here by `encoding.utf8`, independently of the scanner. The first
// continuation byte is where the lead-specific limits live (overlong forms after `E0` and
// `F0`, surrogates after `ED`, code points above U+10FFFF after `F4`), so it walks every
// boundary of those ranges for every lead byte, together with a valid and an invalid final
// continuation byte, in both hex and octal spelling.
fn test_char_literal_byte_escapes_are_one_character_only_when_well_formed_utf8() {
	second_bytes := [u8(0x7f), 0x80, 0x8f, 0x90, 0x9f, 0xa0, 0xbf, 0xc0]
	last_bytes := [u8(0x7f), 0x80, 0xbf, 0xc0]
	mut sequences := [][]u8{}
	for lead in u8(0xc0) .. u8(0xf8) {
		for second in second_bytes {
			if utf8_sequence_len(lead) == 2 {
				sequences << [lead, second]
				continue
			}
			for last in last_bytes {
				mut bytes := [lead, second]
				for bytes.len < utf8_sequence_len(lead) - 1 {
					bytes << u8(0x80)
				}
				bytes << last
				sequences << bytes
			}
		}
	}
	for bytes in sequences {
		well_formed := utf8.validate_str(bytes.bytestr())
		hex := bytes.map('\\x${it.hex()}').join('')
		octal := bytes.map('\\${it:o}').join('')
		for spelling in [hex, octal] {
			source := '`${spelling}`'
			diagnostics := char_literal_diagnostics(source)
			if well_formed {
				assert diagnostics == [], source
			} else {
				assert diagnostics.len == 1, source
				assert diagnostics[0].ends_with('(more than one character)'), '${source}: ${diagnostics[0]}'
			}
		}
	}
}
