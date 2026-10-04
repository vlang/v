module jsonc

// at_opts returns the violation that `validate` finds in `text` under `opts`, or
// nil when it finds none.
fn at_opts(text string, opts ParseOpts) &ParseError {
	return validate(text, opts)
}

// violation_in returns the ParseError that `validate` finds in `text`.
fn violation_in(text string) &ParseError {
	return validate(text, ParseOpts{})
}

fn test_validate_accepts_plain_json() {
	assert violation_in('{"a": 1, "b": [1, 2], "c": {"d": null}}') == nil
	assert violation_in('[]') == nil
	assert violation_in('{}') == nil
	assert violation_in('"a"') == nil
	assert violation_in('1') == nil
	assert violation_in('true') == nil
	assert violation_in('null') == nil
}

fn test_validate_accepts_json_numbers() {
	assert violation_in('{"a": 0}') == nil
	assert violation_in('{"a": -1}') == nil
	assert violation_in('{"a": 1.5}') == nil
	assert violation_in('{"a": -1.5e-3}') == nil
	assert violation_in('{"a": 1E+10}') == nil
	assert violation_in('{"a": 0.5}') == nil
	assert violation_in('{"a": 0e0}') == nil
	assert violation_in('{"a": 1e2}') == nil
	assert violation_in('{"a": -0}') == nil
	assert violation_in('{"a": 10}') == nil
	assert violation_in('{"a": 0.0}') == nil
	assert violation_in('[1, 2, 3]') == nil
}

fn test_validate_rejects_unquoted_keys() {
	assert violation_in('{a: 1}').message.contains('not quoted')
	assert violation_in('{$: 1}').message.contains('not quoted')
	assert violation_in('{a_b: 1}').message.contains('not quoted')
	// A key after a comma is still a key.
	assert violation_in('{"x": 1, b: 2}').message.contains('not quoted')
}

fn test_validate_rejects_non_string_keys() {
	assert violation_in('{1: 1}').message.contains('not quoted')
	assert violation_in('{0x10: 1}').message.contains('not quoted')
	assert violation_in('{true: 1}').message.contains('not quoted')
	assert violation_in('{false: 1}').message.contains('not quoted')
	assert violation_in('{null: 1}').message.contains('not quoted')
	assert violation_in('{Infinity: 1}').message.contains('not quoted')
	assert violation_in('{NaN: 1}').message.contains('not quoted')
}

fn test_validate_rejects_json5_numbers() {
	assert violation_in('{"a": 0x10}').message.contains('hexadecimal')
	assert violation_in('{"a": 0X10}').message.contains('hexadecimal')
	assert violation_in('{"a": .5}').message.contains('leading dot')
	assert violation_in('{"a": +1}').message.contains('leading `+`')
	assert violation_in('{"a": +0x10}').message.contains('leading `+`')
	// A `-` is legal in JSON, so a negative keeps only the form that is not.
	assert violation_in('{"a": -0x10}').message.contains('hexadecimal')
	assert violation_in('{"a": -.5}').message.contains('leading dot')
}

fn test_validate_rejects_incomplete_number_grammar() {
	// RFC 8259 wants a digit after a decimal point and after the `e`, and it does
	// not allow a leading zero. Testing only the last character of the literal
	// misses all of these, because the shared scanner hands every one of them
	// over as a number token.
	assert violation_in('{"a": 5.}').message.contains('no digit after the decimal point')
	assert violation_in('{"a": 1.}').message.contains('no digit after the decimal point')
	assert violation_in('{"a": 1.e2}').message.contains('no digit after the decimal point')
	assert violation_in('{"a": -5.E+2}').message.contains('no digit after the decimal point')
	// `1e` and `1e+` are not here on purpose: the shared scanner refuses them
	// itself, so no number token is ever produced and `validate` returns nil.
	// Through parse_text they are reported by the JSON5 parser, which is covered
	// in jsonc_test.v.
	assert violation_in('{"a": 01}').message.contains('leading zero')
	assert violation_in('{"a": 00.5}').message.contains('leading zero')
	assert violation_in('{"a": -01}').message.contains('leading zero')
	assert violation_in('{"a": 00}').message.contains('leading zero')
	// Inside an array too, where a number is a value rather than a key.
	assert violation_in('[1.e2]').message.contains('no digit after the decimal point')
	assert violation_in('[01]').message.contains('leading zero')
}

fn test_validate_rejects_infinity_and_nan() {
	assert violation_in('{"a": Infinity}').message.contains('not a valid JSON value')
	assert violation_in('{"a": -Infinity}').message.contains('not a valid JSON value')
	assert violation_in('{"a": NaN}').message.contains('not a valid JSON value')
	assert violation_in('[NaN]').message.contains('not a valid JSON value')
	assert violation_in('[Infinity]').message.contains('not a valid JSON value')
}

fn test_validate_rejects_trailing_comma_by_default() {
	assert violation_in('{"a": 1,}').message.contains('trailing comma')
	assert violation_in('{"a": [1,]}').message.contains('trailing comma')
	assert violation_in('[1, 2,]').message.contains('trailing comma')
	// An empty collection has no comma at all.
	assert violation_in('{}') == nil
	assert violation_in('[]') == nil
}

fn test_validate_accepts_trailing_comma_when_asked() {
	opts := ParseOpts{
		allow_trailing_comma: true
	}
	assert at_opts('{"a": 1,}', opts) == nil
	assert at_opts('{"a": [1,]}', opts) == nil
	assert at_opts('[1, 2,]', opts) == nil
	// The option relaxes that one rule and nothing else.
	assert at_opts('{a: 1,}', opts) != nil
	assert at_opts('{"a": 0x10,}', opts) != nil
	assert at_opts('{"a": NaN,}', opts) != nil
}

fn test_validate_does_not_confuse_arrays_with_objects() {
	// A number after a comma inside an array is a value, not an unquoted key.
	assert violation_in('[1, 2, 3]') == nil
	assert violation_in('{"a": [1, {"b": [2]}]}') == nil
	// Closing brackets must pop the right container for the rule to hold.
	assert violation_in('[[1, 2], {"c": 3}]') == nil
	assert violation_in('{"a": [1], "b": 2}') == nil
	assert violation_in('{"a": [{"b": 1}]}') == nil
}

fn test_validate_reports_the_first_violation() {
	v := violation_in('{\n  "a": 0x10,\n  b: 2\n}')
	assert v.message.contains('hexadecimal')
	assert v.pos.line == 2
	assert v.pos.col == 8
}

fn test_validate_positions_recover_byte_offsets() {
	// `{\n  "a": 1,\n  b: 2\n}`: the unquoted `b` is on line 3, column 3, and the
	// byte before it are `{`, a newline, two spaces, `"a": 1,`, a newline and
	// two more spaces.
	v := violation_in('{\n  "a": 1,\n  b: 2\n}')
	assert v.message.contains('not quoted')
	assert v.pos.line == 3
	assert v.pos.col == 3
	assert v.pos.offset == 14
	assert v.pos.end_offset == 15
}

fn test_validate_positions_account_for_a_byte_order_mark() {
	// The JSON5 scanner drops a leading mark before counting, so the column is
	// one less than the byte offset would suggest.
	v := violation_in('\uFEFF{"a": 0x10}')
	assert v.message.contains('hexadecimal')
	assert v.pos.line == 1
	assert v.pos.col == 7
	// Three bytes of mark, then `{"a": ` puts the literal at byte 9.
	assert v.pos.offset == 9
}

fn test_validate_byte_range_covers_the_whole_token() {
	// The range is the token's own source text, so an editor can underline it
	// and a caller can slice the original bytes out of the file.
	mut v := violation_in('{abc: 1}')
	assert v.pos.offset == 1
	assert v.pos.end_offset == 4
	v = violation_in('{"a": 0x10}')
	assert v.pos.offset == 6
	assert v.pos.end_offset == 10
	v = violation_in('{"a": 1,}')
	assert v.pos.offset == 7
	assert v.pos.end_offset == 8
}

fn test_validate_byte_range_covers_a_multibyte_token() {
	// `é` is two bytes, so a range advanced one byte at a time would end inside
	// it and hand back an invalid UTF-8 fragment.
	mut v := violation_in('{é:1}')
	assert v.message.contains('not quoted')
	assert v.pos.offset == 1
	assert v.pos.end_offset == 3
	// The bytes in that range are the character and nothing else.
	assert '{é:1}'[v.pos.offset..v.pos.end_offset] == 'é'
	assert '{é:1}'[v.pos.offset..v.pos.end_offset].bytes() == [195, 169]
	// A longer multibyte key is covered whole as well.
	v = violation_in('{aéb: 1}')
	assert v.pos.offset == 1
	assert v.pos.end_offset == 5
	assert '{aéb: 1}'[v.pos.offset..v.pos.end_offset] == 'aéb'
}

fn test_validate_positions_count_runes_not_bytes() {
	// The two multi-byte characters are legal inside quoted strings, but each
	// pushes the byte offset one further past the column.
	v := violation_in('{"é": "ü", b: 2}')
	assert v.message.contains('not quoted')
	// `{`=1 `"`=2 `é`=3 `"`=4 `:`=5 ` `=6 `"`=7 `ü`=8 `"`=9 `,`=10 ` `=11 `b`=12.
	assert v.pos.col == 12
	// The same character is byte 13, because `é` and `ü` are two bytes each.
	assert v.pos.offset == 13
	assert v.pos.end_offset == 14
}
