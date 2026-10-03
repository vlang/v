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
	assert violation_in('{"a": 5.}').message.contains('trailing dot')
	assert violation_in('{"a": +1}').message.contains('leading `+`')
	assert violation_in('{"a": +0x10}').message.contains('leading `+`')
	// A `-` is legal in JSON, so a negative keeps only the form that is not.
	assert violation_in('{"a": -0x10}').message.contains('hexadecimal')
	assert violation_in('{"a": -.5}').message.contains('leading dot')
	assert violation_in('{"a": -5.}').message.contains('trailing dot')
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
