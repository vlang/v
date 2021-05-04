import x.json2

fn test_json_string_characters() {
	text := json2.raw_decode(r'"\n\r\b\f\t\\\"\/"') or { '' }
	assert text.json_str() == '\\n\\r\\b\\f\\t\\\\\\"\\/'
}

fn test_json_string() {
	text := json2.Any('te✔st')
	assert text.json_str() == r'te\u2714st'
}

fn test_json_string_emoji() {
	text := json2.Any('🐈')
	assert text.json_str() == r' '
}

fn test_json_string_non_ascii() {
	text := json2.Any('ひらがな')
	assert text.json_str() == r'\u3072\u3089\u304c\u306a'
}
