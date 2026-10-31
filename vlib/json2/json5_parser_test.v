module json2

fn test_parse_scalars() {
	assert parse_json5_test('null')! is Null
	assert parse_json5_test('true')!.bool()
	assert !parse_json5_test('false')!.bool()
	assert parse_json5_test('"s"')!.str() == 's'
	assert parse_json5_test("'s'")!.str() == 's'
	assert parse_json5_test('  42  ')!.int() == 42
}

fn test_parse_uppercase_hexadecimal_digits() {
	assert parse_json5_test('0xABCD')!.int() == 0xabcd
	assert parse_json5_test('-0XAB')!.int() == -171
	assert parse_json5_test('"\\uABCD"')!.str() == '\uABCD'
	assert parse_json5_test('"\\xAB"')!.str() == '\u00AB'
}

fn test_parse_utf16_surrogate_pairs_in_strings_and_keys() {
	for text in [r'"\uD83C\uDFBC"', r"'\ud83c\udfbc'", r'"\u{1F3BC}"'] {
		assert parse_json5_test(text)!.str() == '🎼'
		assert parse_json5_test(text)!.str().bytes() == [u8(0xf0), 0x9f, 0x8e, 0xbc]
	}
	assert parse_json5_test(r'"a\uD83C\uDFBC\uD83D\uDE00z"')!.str() == 'a🎼😀z'
	assert parse_json5_test(r'"\uDBFF\uDFFF"')!.str() == rune(0x10ffff).str()
	object := parse_json5_test(r'{"\uD83C\uDFBC": "\uD83D\uDE00"}')!.as_map()
	assert (object['🎼'] or { panic('missing decoded key') }).str() == '😀'
}

fn test_parse_rejects_malformed_surrogate_escapes() {
	for text in [r'"\uD83C"', r'"\uDFBC"', r'"\uD83C\u0041"', r'"\uD83C\uD83C"', r'"\uD83Cx\uDFBC"',
		r'"\uD83C\uDFB"', r'"\u{D83C}"'] {
		parse_json5_test(text) or { continue }
		assert false, 'expected invalid surrogate escape to fail: ${text}'
	}
}

fn test_parse_empty_object_and_array() {
	obj := parse_json5_test('{}')!
	assert obj is map[string]Any
	assert (obj as map[string]Any).len == 0
	arr := parse_json5_test('[]')!
	assert arr is []Any
	assert (arr as []Any).len == 0
}

fn test_parse_nested_object() {
	doc := parse_json5_doc_test('{ a: { b: { c: 1 } } }')!
	assert doc.value('a.b.c').int() == 1
}

fn test_parse_array_of_objects() {
	doc := parse_json5_doc_test('[{ id: 1 }, { id: 2 }]')!
	assert doc.value('[0].id').int() == 1
	assert doc.value('[1].id').int() == 2
}

fn test_parse_quoted_key() {
	doc := parse_json5_doc_test('{ "a b": 1 }')!
	assert doc.value('a b').int() == 1
}

fn test_parse_quoted_numeric_keys_and_bare_keyword_keys() {
	doc := parse_json5_doc_test('{ "0x10": 1, \'42\': 2, null: 3, true: 4, Infinity: 5,
		false: 6, NaN: 7 }')!
	assert doc.value('0x10').int() == 1
	assert doc.value('42').int() == 2
	assert doc.value('null').int() == 3
	assert doc.value('true').int() == 4
	assert doc.value('Infinity').int() == 5
	assert doc.value('false').int() == 6
	assert doc.value('NaN').int() == 7
}

fn test_parse_numeric_key_errors_keep_original_positions() {
	for key in ['42', '0x10', '.5', '-42', '+Infinity', '-NaN'] {
		parse_json5_doc_test('// heading\r\n{\n  ' + key + ': 1}') or {
			assert err is JsonScanError
			perr := err as JsonScanError
			assert perr.message == 'unexpected `${key}`, expected an object key'
			assert perr.line == 3
			assert perr.character == 3
			continue
		}
		assert false, 'expected numeric object key to fail: ${key}'
	}
}

fn test_parse_trailing_comma_in_object() {
	doc := parse_json5_doc_test('{ a: 1, b: 2, }')!
	assert doc.value('a').int() == 1
	assert doc.value('b').int() == 2
}

fn test_parse_trailing_comma_in_array() {
	doc := parse_json5_doc_test('[1, 2, ]')!
	assert doc.value('[0]').int() == 1
	assert doc.value('[1]').int() == 2
}

fn test_parse_comments_everywhere() {
	doc := parse_json5_doc_test('// leading
		{ // after brace
			a: 1, // after value
			/* block */ b: 2,
		} // trailing')!
	assert doc.value('a').int() == 1
	assert doc.value('b').int() == 2
}

fn test_parse_multi_line_block_comment() {
	doc := parse_json5_doc_test('{
		a: 1, /* line one
			line two */
		b: 2,
	}')!
	assert doc.value('b').int() == 2
}

fn test_parse_last_duplicate_key_wins() {
	doc := parse_json5_doc_test('{ a: 1, a: 2 }')!
	assert doc.value('a').int() == 2
}

fn test_parse_nan_and_infinity() {
	items := parse_json5_test('[NaN, Infinity, -Infinity]')! as []Any
	assert items[0].f64() != items[0].f64()
	assert items[1].f64() > 0.0
	assert items[2].f64() < 0.0
}

fn test_parse_error_missing_value() {
	parse_json5_test('{ a: }') or {
		assert err is JsonScanError
		return
	}
	assert false, 'expected a parse error'
}

fn test_parse_error_missing_colon() {
	parse_json5_test('{ a 1 }') or {
		assert err.msg().contains('expected `:`')
		return
	}
	assert false, 'expected a missing colon error'
}

fn test_parse_error_unclosed_object() {
	parse_json5_test('{ a: 1') or {
		assert err.msg().contains('unexpected end of input')
		return
	}
	assert false, 'expected an unclosed object error'
}

fn test_parse_error_trailing_garbage() {
	parse_json5_test('{} extra') or {
		assert err is JsonScanError
		return
	}
	assert false, 'expected a trailing garbage error'
}

fn test_parse_error_reports_position() {
	parse_json5_test('{\n  a: 1,\n  b: ,\n}') or {
		perr := err as JsonScanError
		assert perr.line == 3
		assert perr.character > 1
		return
	}
	assert false, 'expected a parse error with a position'
}

fn test_parse_empty_input() {
	parse_json5_test('') or {
		assert err.msg().contains('expected a value')
		return
	}
	assert false, 'expected an empty input error'
}

fn test_parse_bom_is_skipped() {
	assert parse_json5_doc_test('\uFEFF{ a: 1 }')!.value('a').int() == 1
}

fn parse_json5_test(text string) !Any {
	return decode[Any](text, json5: true)
}

fn parse_json5_doc_test(text string) !Doc {
	return parse_text(text, json5: true)
}
