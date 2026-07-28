module scanner

import v3.pref
import v3.token

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

	for scanner.scan() != .eof {}
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

		for scanner.scan() != .eof {}
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
