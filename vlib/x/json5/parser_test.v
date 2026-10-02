module json5

fn test_parse_scalars() {
	assert parse('null')! is Null
	assert parse('true')!.bool()
	assert !parse('false')!.bool()
	assert parse('"s"')!.string() == 's'
	assert parse("'s'")!.string() == 's'
	assert parse('  42  ')!.int() == 42
}

fn test_parse_uppercase_hexadecimal_digits() {
	assert parse('0xABCD')!.int() == 0xabcd
	assert parse('-0XAB')!.int() == -171
	assert parse('"\\uABCD"')!.string() == '\uABCD'
	assert parse('"\\xAB"')!.string() == '\u00AB'
}

fn test_parse_utf16_surrogate_pairs_in_strings_and_keys() {
	for text in [r'"\uD83C\uDFBC"', r"'\ud83c\udfbc'", r'"\u{1F3BC}"'] {
		assert parse(text)!.string() == '🎼'
		assert parse(text)!.string().bytes() == [u8(0xf0), 0x9f, 0x8e, 0xbc]
	}
	assert parse(r'"a\uD83C\uDFBC\uD83D\uDE00z"')!.string() == 'a🎼😀z'
	assert parse(r'"\uDBFF\uDFFF"')!.string() == rune(0x10ffff).str()
	object := parse(r'{"\uD83C\uDFBC": "\uD83D\uDE00"}')!.as_map()
	assert (object['🎼'] or { panic('missing decoded key') }).string() == '😀'
}

fn test_parse_rejects_malformed_surrogate_escapes() {
	for text in [r'"\uD83C"', r'"\uDFBC"', r'"\uD83C\u0041"', r'"\uD83C\uD83C"', r'"\uD83Cx\uDFBC"',
		r'"\uD83C\uDFB"', r'"\u{D83C}"'] {
		parse(text) or { continue }
		assert false, 'expected invalid surrogate escape to fail: ${text}'
	}
}

fn test_parse_empty_object_and_array() {
	obj := parse('{}')!
	assert obj is map[string]Any
	assert (obj as map[string]Any).len == 0
	arr := parse('[]')!
	assert arr is []Any
	assert (arr as []Any).len == 0
}

fn test_parse_nested_object() {
	doc := parse_text('{ a: { b: { c: 1 } } }')!
	assert doc.value('a.b.c').int() == 1
}

fn test_parse_array_of_objects() {
	doc := parse_text('[{ id: 1 }, { id: 2 }]')!
	assert doc.value('[0].id').int() == 1
	assert doc.value('[1].id').int() == 2
}

fn test_parse_quoted_key() {
	doc := parse_text('{ "a b": 1 }')!
	assert doc.value('a b').int() == 1
}

fn test_parse_numeric_looking_keys() {
	doc := parse_text('{ 0x10: 1, 42: 2, null: 3, true: 4, Infinity: 5 }')!
	assert doc.value('0x10').int() == 1
	assert doc.value('42').int() == 2
	assert doc.value('null').int() == 3
	assert doc.value('true').int() == 4
	assert doc.value('Infinity').int() == 5
}

fn test_parse_trailing_comma_in_object() {
	doc := parse_text('{ a: 1, b: 2, }')!
	assert doc.value('a').int() == 1
	assert doc.value('b').int() == 2
}

fn test_parse_trailing_comma_in_array() {
	doc := parse_text('[1, 2, ]')!
	assert doc.value('[0]').int() == 1
	assert doc.value('[1]').int() == 2
}

fn test_parse_comments_everywhere() {
	doc := parse_text('// leading
		{ // after brace
			a: 1, // after value
			/* block */ b: 2,
		} // trailing')!
	assert doc.value('a').int() == 1
	assert doc.value('b').int() == 2
}

fn test_parse_multi_line_block_comment() {
	doc := parse_text('{
		a: 1, /* line one
			line two */
		b: 2,
	}')!
	assert doc.value('b').int() == 2
}

fn test_parse_last_duplicate_key_wins() {
	doc := parse_text('{ a: 1, a: 2 }')!
	assert doc.value('a').int() == 2
}

fn test_parse_nan_and_infinity() {
	items := parse('[NaN, Infinity, -Infinity]')! as []Any
	assert items[0].f64() != items[0].f64()
	assert items[1].f64() > 0.0
	assert items[2].f64() < 0.0
}

fn test_parse_error_missing_value() {
	parse('{ a: }') or {
		assert err is ParseError
		return
	}
	assert false, 'expected a parse error'
}

fn test_parse_error_missing_colon() {
	parse('{ a 1 }') or {
		assert err.msg().contains('expected `:`')
		return
	}
	assert false, 'expected a missing colon error'
}

fn test_parse_error_unclosed_object() {
	parse('{ a: 1') or {
		assert err.msg().contains('unexpected end of input')
		return
	}
	assert false, 'expected an unclosed object error'
}

fn test_parse_error_trailing_garbage() {
	parse('{} extra') or {
		assert err is ParseError
		return
	}
	assert false, 'expected a trailing garbage error'
}

fn test_parse_error_reports_position() {
	parse('{\n  a: 1,\n  b: ,\n}') or {
		perr := err as ParseError
		assert perr.line == 3
		assert perr.col > 1
		return
	}
	assert false, 'expected a parse error with a position'
}

fn test_parse_empty_input() {
	parse('') or {
		assert err.msg().contains('expected a value')
		return
	}
	assert false, 'expected an empty input error'
}

fn test_parse_bom_is_skipped() {
	assert parse_text('\uFEFF{ a: 1 }')!.value('a').int() == 1
}
