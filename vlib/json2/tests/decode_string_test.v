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

// Lone `\uXXXX` surrogate escapes decode to their WTF-8 3-byte form instead of
// erroring (see https://github.com/vlang/v/issues/32389). RFC 8259 section 7
// allows any `\uXXXX` escape, JavaScript `JSON.parse` accepts lone surrogates,
// and `JSON.stringify` can emit them, so rejecting them made such input
// unreadable. A head surrogate directly followed by a trail surrogate escape
// still decodes as one code point (see test_surrogate).
fn test_lone_surrogate_decodes_as_wtf8() {
	assert json.decode[string](r'"\ud800"')!.bytes().hex() == 'eda080'
	assert json.decode[string](r'"\ud83d"')!.bytes().hex() == 'eda0bd'
	assert json.decode[string](r'"\udc00"')!.bytes().hex() == 'edb080'
	assert json.decode[string](r'"\ude00"')!.bytes().hex() == 'edb880'
	assert json.decode[string](r'"a\udc00b"')!.bytes().hex() == '61edb08062'
	// A head surrogate only joins a trail surrogate escape directly after it;
	// otherwise each half is kept on its own, like JavaScript engines do.
	assert json.decode[string](r'"\ud83d\ud83d\ude00"')!.bytes().hex() == 'eda0bdf09f9880'
	assert json.decode[string](r'"\ud83d\ude00\ude00"')!.bytes().hex() == 'f09f9880edb880'
	assert json.decode[string](r'"\ud83d\n\n\n\n"')!.bytes().hex() == 'eda0bd0a0a0a0a'
	assert json.decode[string](r'"\ud83dx\ude00"')!.bytes().hex() == 'eda0bd78edb880'
	assert json.decode[string](r'"\ud83d\u0041"')!.bytes().hex() == 'eda0bd41'
	// The report in #32389, via the generic value type.
	value := json.decode[json.Any]('["\\ud800", "a\\udc00b"]')!
	assert value.as_array().map(it.str().bytes().hex()) == ['eda080', '61edb08062']
}

// A preserved WTF-8 surrogate must encode as `\uXXXX` in both default and
// `escape_unicode` modes, so decode/encode/decode round-trips instead of
// emitting invalid UTF-8 (default) or three U+FFFD (escaped).
fn test_lone_surrogate_encode_roundtrip() {
	for escape_unicode in [false, true] {
		// Lone units keep their lowercase escape in both modes.
		for raw in [r'"\ud800"', r'"\udc00"', r'"a\udc00b"'] {
			decoded := json.decode[string](raw)!
			encoded := json.encode(decoded, escape_unicode: escape_unicode)
			assert encoded == raw
			assert json.decode[string](encoded)!.bytes().hex() == decoded.bytes().hex()
		}
		// A lone head before a valid pair escapes on its own; the pair stays
		// one code point (raw emoji by default, surrogate pair when escaped).
		combo := json.decode[string](r'"\ud83d\ud83d\ude00"')!
		assert combo.bytes().hex() == 'eda0bdf09f9880'
		encoded_combo := json.encode(combo, escape_unicode: escape_unicode)
		if escape_unicode {
			assert encoded_combo == r'"\ud83d\uD83D\ude00"'
		} else {
			assert encoded_combo == '"\\ud83d😀"'
		}
		assert json.decode[string](encoded_combo)!.bytes().hex() == combo.bytes().hex()
		// Valid-pair control: no lone unit, so default keeps raw UTF-8.
		pair := json.decode[string](r'"\ud83d\ude00"')!
		assert pair.bytes().hex() == 'f09f9880'
		encoded_pair := json.encode(pair, escape_unicode: escape_unicode)
		if escape_unicode {
			assert encoded_pair == r'"\uD83D\ude00"'
		} else {
			assert encoded_pair == '"😀"'
		}
		assert json.decode[string](encoded_pair)! == pair
		// The generic value type carries the same WTF-8 bytes as strings.
		any_value := json.decode[json.Any](r'"\ud800"')!
		encoded_any := json.encode(any_value, escape_unicode: escape_unicode)
		assert encoded_any == r'"\ud800"'
		assert json.decode[json.Any](encoded_any)!.str().bytes().hex() == 'eda080'
		// Map keys go through the same string encoder, so they round-trip too.
		key_map := json.decode[map[string]string](r'{"\ud800":"v","a\udc00b":"w","ok":"x"}')!
		assert key_map.keys().len == 3
		encoded_map := json.encode(key_map, escape_unicode: escape_unicode)
		assert encoded_map.contains(r'\ud800')
		assert encoded_map.contains(r'\udc00')
		assert json.decode[map[string]string](encoded_map)! == key_map
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
