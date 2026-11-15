import json2 as json

fn test_json_escape_low_chars() {
	assert json.decode[string](r'"\u001b"')! == '\u001b'
	assert json.decode[string](r'"\u000f"')! == '\u000f'
	assert json.decode[string](r'" "')! == '\u0020'
	assert json.decode[string](r'"\u0000"')! == '\u0000'
}

fn test_decode_escaped_string_round_trip() {
	escaped_strings := ['test', 'test\\sd', 'test\nsd', '\ntest', 'test\\"', 'test\\', 'test\u1234ps',
		'test\u1234', '\u1234\\\t"', '', '\uff0f', 'test \uff0f test', '😀', 'text 😀 text']

	json_string := json.encode[[]string](escaped_strings)
	decoded_strings := json.decode[[]string](json_string)!

	assert escaped_strings == decoded_strings
}

fn test_surrogate() {
	assert json.decode[string](r'"\ud83d\ude00"')! == '😀'
	assert json.decode[string](r'"\ud83d\ude00 text"')! == '😀 text'
}

fn test_invalid_surrogate() {
	if _ := json.decode[string](r'"\ud83d"') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected a trail surrogate after a head surrogate, but got no valid escape sequence.'
		}
	}

	if _ := json.decode[string](r'"\ud83d\n\n\n\n"') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected a trail surrogate after a head surrogate, but got no valid escape sequence.'
		}
	}

	if _ := json.decode[string](r'"\ud83d\ud83d"') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected a trail surrogate after a head surrogate, but got D83D.'
		}
	}

	if _ := json.decode[string](r'"\ude00\ud83d"') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Got trail surrogate: DE00 before head surrogate.'
		}
	}
}

struct Inner {
mut:
	ok string
}

struct Outer {
mut:
	strings map[string]string
	inner   Inner
}

struct Base {
mut:
	id int
}

struct WithEmbed {
	Base
mut:
	name string
}

fn test_escaped_struct_and_map_keys() {
	o := json.decode[Outer](r'{"str\u0069ngs":{"o\u006b":"OK"},"\u0069nner":{"ok":"yes"}}')!
	assert o.strings['ok'] == 'OK'
	assert o.inner.ok == 'yes'
}

fn test_escaped_key_with_embed() {
	w := json.decode[WithEmbed](r'{"\u0069d": 7, "nam\u0065": "x"}')!
	assert w.id == 7
	assert w.name == 'x'
}

fn test_escaped_map_keys() {
	m := json.decode[map[string]string](r'{"\u006bey": "value", "a\"b\n": "c"}')!
	assert m['key'] == 'value'
	assert m['a"b\n'] == 'c'
	assert m.len == 2
}

fn test_unescaped_keys_still_match() {
	o := json.decode[Outer]('{"strings":{"ok":"OK"},"inner":{"ok":"yes"}}')!
	assert o.strings['ok'] == 'OK'
	assert o.inner.ok == 'yes'
}

fn test_escaped_key_roundtrip() {
	original := {
		'a"b\n': 1
	}
	assert json.decode[map[string]int](json.encode(original))! == original
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
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 3
			assert err.message == 'Syntax: unknown escape sequence'
		}
		has_error = true
	} // Invalid escape

	assert has_error, 'Expected error'
	has_error = false

	json.decode[string](r'"\u123"') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 3
			assert err.message == 'Syntax: short unicode escape sequence \\u123'
		}
		has_error = true
	} // Incomplete Unicode

	assert has_error, 'Expected error'
}

fn test_json_string_whitespace() {
	// Test strings with whitespace
	assert json.decode[string]('"   "')! == '   '
	assert json.decode[string]('"\t\n\r"')! == '\t\n\r'
}
