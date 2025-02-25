import x.json2.decoder2 as json

fn test_json_escape_low_chars() {
	assert json.decode[string](r'"\u001b"')! == '\u001b'
	assert json.decode[string](r'"\u000f"')! == '\u000f'
	assert json.decode[string](r'" "')! == '\u0020'
	assert json.decode[string](r'"\u0000"')! == '\u0000'
}

fn test_json_string() {
	assert json.decode[string](r'"te\u2714st"')! == 'te✔st'
	assert json.decode[string](r'"te✔st"')! == 'te✔st'
	assert json.decode[string]('""')! == ''
}

fn test_json_string_emoji() {
	assert json.decode[string](r'"🐈"')! == '🐈'
	assert json.decode[string](r'"💀"')! == '💀'
	assert json.decode[string](r'"🐈💀"')! == '🐈💀'
}

fn test_json_string_non_ascii() {
	assert json.decode[string](r'"\u3072\u3089\u304c\u306a"')! == 'ひらがな'
	assert json.decode[string]('"a\\u3072b\\u3089c\\u304cd\\u306ae fgh"')! == 'aひbらcがdなe fgh'
	assert json.decode[string]('"\\u3072\\u3089\\u304c\\u306a"')! == 'ひらがな'
}

fn test_utf8_strings_are_not_modified() {
	assert json.decode[string]('"ü"')! == 'ü'
	assert json.decode[string]('"Schilddrüsenerkrankungen"')! == 'Schilddrüsenerkrankungen'
}

fn test_json_string_invalid_escapes() {
	mut has_error := false

	json.decode[string](r'"\x"') or {
		assert err.msg() == '\n"\\\n ^ unknown escape sequence'
		has_error = true
	} // Invalid escape

	assert has_error, 'Expected error'
	has_error = false

	json.decode[string](r'"\u123"') or {
		assert err.msg() == '\n"\\\n ^ short unicode escape sequence \\u123"'
		has_error = true
	} // Incomplete Unicode

	assert has_error, 'Expected error'
}

fn test_json_string_whitespace() {
	// Test strings with whitespace
	assert json.decode[string]('"   "')! == '   '
	assert json.decode[string]('"\t\n\r"')! == '\t\n\r'
}
