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

fn assert_json5_parser_accepts_typed_input(text string) {
	if _ := decode[int](text, json5: true) {
		assert false, 'container input must not decode as a number'
	} else {
		assert err is JsonDecodeError
		assert !err.msg().contains('maximum nesting depth')
	}
}

fn test_json5_nesting_at_max_depth_is_accepted() {
	arrays := '['.repeat(max_nesting_depth) + '0' + ']'.repeat(max_nesting_depth)
	assert_json5_parser_accepts_typed_input(arrays)

	objects := '{key:'.repeat(max_nesting_depth) + '0' + '}'.repeat(max_nesting_depth)
	assert_json5_parser_accepts_typed_input(objects)
	assert_json5_parser_accepts_typed_input(mixed_json5_nested_value(max_nesting_depth))
}

fn mixed_json5_nested_value(levels int) string {
	mut parts := []string{cap: levels * 2 + 1}
	for i in 0 .. levels {
		parts << if i % 2 == 0 { '[' } else { '{key:' }
	}
	parts << '0'
	for i := levels - 1; i >= 0; i-- {
		parts << if i % 2 == 0 { ']' } else { '}' }
	}
	return parts.join('')
}

fn commented_json5_nested_array(levels int) string {
	mut parts := []string{cap: levels * 2 + 1}
	for _ in 0 .. levels {
		parts << '['
		parts << '/* nested */'
	}
	parts << '0'
	for _ in 0 .. levels {
		parts << ',]'
	}
	return parts.join('')
}

fn assert_json5_parser_depth_error(text string, character int) {
	if _ := decode[int](text, json5: true) {
		assert false, 'expected JSON5 nesting over the limit to return an error'
	} else {
		assert err is JsonScanError
		scan_error := err as JsonScanError
		assert scan_error.msg().contains('maximum nesting depth')
		assert scan_error.line == 1
		assert scan_error.character == character
	}
}

fn test_json5_nesting_over_max_depth_returns_error() {
	arrays := '['.repeat(max_nesting_depth + 1) + '0' + ']'.repeat(max_nesting_depth + 1)
	objects := '{key:'.repeat(max_nesting_depth + 1) + '0' + '}'.repeat(max_nesting_depth + 1)
	assert_json5_parser_depth_error(arrays, max_nesting_depth + 1)
	assert_json5_parser_depth_error(objects, max_nesting_depth * '{key:'.len + 1)
	mixed_error_character := max_nesting_depth / 2 * ('['.len + '{key:'.len) + 1
	assert_json5_parser_depth_error(mixed_json5_nested_value(max_nesting_depth + 1), mixed_error_character)
	commented_error_character := max_nesting_depth * ('['.len + '/* nested */'.len) + 1
	assert_json5_parser_depth_error(commented_json5_nested_array(max_nesting_depth + 1), commented_error_character)
}

fn test_json5_typed_decode_rejects_over_parser_limit() {
	text := '['.repeat(max_nesting_depth + 1) + '0' + ']'.repeat(max_nesting_depth + 1)
	assert_json5_parser_depth_error(text, max_nesting_depth + 1)
}

fn test_json5_nesting_guard_covers_typed_and_reused_decoding() {
	text := '{key:'.repeat(max_nesting_depth + 1) + '0' + '}'.repeat(max_nesting_depth + 1)
	if _ := decode[int](text, json5: true) {
		assert false, 'expected typed JSON5 decoding to reject over-limit nesting'
	} else {
		assert err.msg().contains('maximum nesting depth')
	}
	mut buffer := DecodeBuffer{}
	decode_reuse[int](text, mut buffer, json5: true) or {
		assert err.msg().contains('maximum nesting depth')
		value := decode_reuse[Any]('{key: [1]}', mut buffer, json5: true)!
		assert value is map[string]Any
		return
	}
	assert false, 'expected JSON5 decode_reuse to reject over-limit nesting'
}

fn test_json5_nesting_counts_tokens_not_delimiter_text() {
	text := "['" + '['.repeat(max_nesting_depth + 1) + "']"
	value := decode[Any](text, json5: true) or { panic('string delimiters are not nesting: ${err.msg()}') }
	assert value is []Any
	assert (value as []Any)[0].str() == '['.repeat(max_nesting_depth + 1)
}

fn test_json5_nesting_guard_unwinds_between_sibling_containers() {
	sub := '['.repeat(max_nesting_depth - 1) + '0' + ']'.repeat(max_nesting_depth - 1)
	assert_json5_parser_accepts_typed_input('[${sub}, ${sub}]')
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
