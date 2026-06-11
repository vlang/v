import x.json2 as json

fn must_fail_decode_any(input string) {
	if _ := json.decode[json.Any](input, strict: true) {
		assert false
	}
}

fn test_rfc8259_top_level_values() {
	assert json.decode[json.Any]('null', strict: true)! is json.Null
	assert json.decode[json.Any]('true', strict: true)! == json.Any(true)
	assert json.decode[json.Any]('false', strict: true)! == json.Any(false)
	assert json.decode[json.Any]('0', strict: true)! == json.Any(i64(0))
	assert json.decode[json.Any]('"x"', strict: true)! == json.Any('x')
	assert json.decode[json.Any]('[]', strict: true)! == json.Any([]json.Any{})
	assert json.decode[json.Any]('{}', strict: true)! == json.Any(map[string]json.Any{})
}

fn test_rfc8259_rejects_trailing_data() {
	must_fail_decode_any('1 2')
	must_fail_decode_any('{}[]')
	must_fail_decode_any('true false')
	must_fail_decode_any('"x"0')
}

fn test_rfc8259_rejects_empty_and_common_syntax_errors() {
	must_fail_decode_any('')
	must_fail_decode_any('[1,]')
	must_fail_decode_any('{"a":1,}')
	must_fail_decode_any('01')
}

fn test_rfc8259_rejects_unescaped_control_character_in_string() {
	bad := '{"s":"a' + '\x01' + 'b"}'
	if _ := json.decode[map[string]json.Any](bad, strict: true) {
		assert false
	}
}

fn test_rfc8259_utf16_surrogates() {
	smile := json.decode[string](r'"\uD83D\uDE00"', strict: true)!
	assert smile.runes().len == 1
	assert smile.runes()[0] == rune(0x1f600)

	if _ := json.decode[string](r'"\uDE00"', strict: true) {
		assert false
	}
	if _ := json.decode[string](r'"\uD83D\u0041"', strict: true) {
		assert false
	}
}

fn test_rfc8259_any_preserves_integer_precision_when_possible() {
	max_i64 := json.decode[json.Any]('9223372036854775807', strict: true)!
	match max_i64 {
		i64 { assert max_i64 == i64(9223372036854775807) }
		else { assert false }
	}

	beyond_i64 := json.decode[json.Any]('9223372036854775808', strict: true)!
	match beyond_i64 {
		u64 { assert beyond_i64 == u64(9223372036854775808) }
		else { assert false }
	}

	beyond_u64 := json.decode[json.Any]('18446744073709551616', strict: true)!
	match beyond_u64 {
		f64 { assert beyond_u64 > 0 }
		else { assert false }
	}
}
