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
	assert scanner.diagnostics.len == 2
	assert scanner.diagnostics[0].message == 'identifier name `3a` cannot start with a number'
	assert scanner.diagnostics[1].message == 'identifier name `4b` cannot start with a number'
	assert scanner.diagnostics[0].offset == 0
	assert scanner.diagnostics[0].end == 2
	assert scanner.diagnostics[1].offset == 8
	assert scanner.diagnostics[1].end == 10
}
