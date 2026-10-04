module jsonc

// accepted asserts that `text` parses as JSONC.
fn accepted(text string) {
	parse_text(text) or {
		assert false, 'expected `${text}` to be accepted, got: ${err.msg()}'
		return
	}
}

// rejected asserts that `text` is refused with a JSONC strictness violation,
// and returns the message so a caller can check it.
fn rejected(text string) string {
	parse_text(text) or {
		return err.msg()
	}
	assert false, 'expected `${text}` to be rejected'
	return ''
}

// first_violation returns the strictness violation `lex` finds in `text`, or an
// empty string when it finds none.
fn first_violation(text string) string {
	res := lex(text)
	found := res.violation
	if found == nil {
		return ''
	}
	return found.msg()
}

// lex_violation returns the ParseError that `lex` finds in `text`.
fn lex_violation(text string) &ParseError {
	res := lex(text)
	found := res.violation
	assert found != nil, 'expected a violation in `${text}`'
	return found
}

fn test_lex_accepts_plain_strings() {
	assert first_violation('{"a": "b"}') == ''
	assert first_violation('""') == ''
	assert first_violation('["", ""]') == ''
}

fn test_lex_accepts_every_rfc_escape() {
	// The seven escapes RFC 8259 allows without an argument, plus \uXXXX.
	assert first_violation('{"a": "\\"\\\\\\/\\b\\f\\n\\r\\t"}') == ''
	assert first_violation('{"a": "\\u00e9"}') == ''
	assert first_violation('{"a": "\\uFFFF"}') == ''
	assert first_violation('{"a": "\\u0000"}') == ''
}

fn test_lex_rejects_single_quoted_string() {
	assert rejected("{'a': 1}").contains('single-quoted string')
	assert rejected("['a']").contains('single-quoted string')
	// The position is the opening quote, and the range is the whole literal.
	assert rejected("{'a': 1}").contains('jsonc: 1:2:')
	v := lex_violation("{'a': 1}")
	assert v.pos.offset == 1
	assert v.pos.end_offset == 4
}

fn test_lex_rejects_escapes_outside_rfc() {
	assert rejected('{"a": "\\v"}').contains('not a valid JSONC escape')
	assert rejected('{"a": "\\0"}').contains('not a valid JSONC escape')
	assert rejected('{"a": "\\x41"}').contains('not a valid JSONC escape')
	assert rejected('{"a": "\\\'"}').contains('not a valid JSONC escape')
	// A JSON5 escape of any other character yields the character itself.
	assert rejected('{"a": "\\q"}').contains('not a valid JSONC escape')
}

fn test_lex_rejects_es6_unicode_escape() {
	// JSON5 accepts the ES6 `\u{...}` form, so this is the case the lexical pass
	// has to catch on its own.
	assert rejected('{"a": "\\u{1F600}"}').contains('four hexadecimal digits')
	// A short `\u` is refused by the JSON5 scanner before the pass sees it, so
	// only the fact that it is refused matters here.
	assert rejected('{"a": "\\u12"}').contains('hexadecimal')
}

fn test_lex_rejects_escaped_line_break() {
	assert rejected('{"a": "x\\\ny"}').contains('escaped line break')
}

fn test_lex_rejects_raw_control_character() {
	assert rejected('{"a": "x\ny"}').contains('control character')
	assert rejected('{"a": "x\ty"}').contains('control character')
	// A tab inside a string is a control character, unlike a tab between tokens.
	assert first_violation('{\t"a": 1}') == ''
}

fn test_lex_ignores_comment_contents() {
	// Quotes, escapes and slashes inside a comment are not string syntax.
	assert first_violation('{/* it\'s a "quote" */ "a": 1}') == ''
	assert first_violation('{/* \\uZZZZ */ "a": 1}') == ''
	assert first_violation('{// \'single\'\n"a": 1}') == ''
	assert first_violation('{/* /* nested */ */ "a": 1}') == ''
}

fn test_lex_ignores_string_contents_for_comments() {
	// A `//` inside a string is not a comment, so the quote after it closes.
	assert first_violation('{"a": "http://x"}') == ''
	assert first_violation('{"a": "/* not a comment */"}') == ''
}

fn test_lex_stops_at_unterminated_string() {
	// The JSON5 parser owns malformed input, so the pass must not report a
	// dialect problem for a literal it could not finish.
	assert first_violation('{"a": "unterminated') == ''
	assert first_violation('/* unterminated') == ''
	assert first_violation('{"a": 1') == ''
}

// ch returns `code` as a one-character string, so a test can name a code point
// without depending on how the source file encodes it.
fn ch(code int) string {
	return rune(code).str()
}

fn test_lex_accepts_rfc_whitespace() {
	assert first_violation('{"a"' + ch(0x20) + ':' + ch(0x09) + '1' + ch(0x0A) +
		'}') == ''
	// An empty document is only whitespace, and it is the parser that refuses it.
	assert first_violation(ch(0x20) + ch(0x0A) + ch(0x0D) + ch(0x09)) == ''
}

fn test_lex_rejects_json5_only_whitespace() {
	// The shared scanner skips all of these as trivia, so the token pass never
	// sees them and the raw walk is the only place they can be caught. Vertical
	// tab and form feed are the ASCII pair and U+00A0 is named in the shared
	// `is_whitespace`; the rest are Unicode spaces above U+2000.
	for code in [0x0B, 0x0C, 0xA0, 0x2028, 0x2029, 0x202F, 0x205F] {
		assert first_violation('{"a"' + ch(code) + ': 1}').contains('not whitespace between JSON tokens')
	}
}

fn test_lex_rejects_an_interior_byte_order_mark() {
	// The documented exception is a leading mark, which new_cursor steps over.
	assert first_violation(ch(0xFEFF) + '{"a": 1}') == ''
	assert first_violation('{"a"' + ch(0xFEFF) + ': 1}').contains('U+FEFF')
	assert first_violation('{"a": 1' + ch(0xFEFF) + '}').contains('U+FEFF')
}

fn test_lex_allows_json5_whitespace_inside_comments_and_strings() {
	// Inside a comment the text is opaque, and inside a string the string rules
	// decide, so none of these is a whitespace question.
	for code in [0x0B, 0x0C, 0xA0, 0x3000] {
		assert first_violation('{/*' + ch(code) + '*/"a": 1}') == ''
		assert first_violation('{//' + ch(code) + '\n"a": 1}') == ''
	}
	assert first_violation('{"a": "' + ch(0xA0) + '"}') == ''
}

fn test_strip_comments_replaces_with_spaces() {
	// The expectations are stated as properties rather than as counted runs of
	// spaces, so the test does not depend on how wide the comment was.
	mut out := strip_comments('{"a": 1 /* c */}')
	assert out.len == 16
	assert out.starts_with('{"a": 1 ')
	assert out.ends_with('}')
	assert !out.contains('c')

	out = strip_comments('{"a": 1} // tail')
	assert out.len == 16
	assert out.starts_with('{"a": 1}')
	assert !out.contains('t')

	assert strip_comments('// only') == '       '
	assert strip_comments('/* only */') == '          '
	assert strip_comments('{"a": 1}') == '{"a": 1}'
}

fn test_strip_comments_preserves_byte_offsets() {
	text := '{\n  "a": 1, // one\n  /* two */ "b": 2\n}'
	out := strip_comments(text)
	assert out.len == text.len
	// The line terminators are kept, so lines do not move.
	assert out.split_into_lines().len == text.split_into_lines().len
	// And the result is still the same document.
	assert parse_text(out) or { panic(err) }.str() == '{"a":1,"b":2}'
}

fn test_strip_comments_leaves_strings_alone() {
	assert strip_comments('{"u": "http://a//b"}') == '{"u": "http://a//b"}'
	assert strip_comments('{"a": "/* not */"}') == '{"a": "/* not */"}'
	assert strip_comments('{"a": 1}') == '{"a": 1}'
}

fn test_strip_comments_handles_unterminated_block_comment() {
	// Everything after the opening delimiter is comment, to the end of input.
	out := strip_comments('{"a": 1 /* rest')
	assert out.len == 15
	assert out.starts_with('{"a": 1 ')
	assert !out.contains('r')
	assert !out.contains('e')
	assert !out.contains('s')
	assert !out.contains('t')
}

fn test_strip_comments_handles_bom() {
	// The byte order mark is content, not a comment.
	assert strip_comments('\uFEFF{"a": 1}') == '\uFEFF{"a": 1}'
}

fn test_lex_positions_count_runes_not_bytes() {
	// A multi-byte character before the violation shifts the byte offset by more
	// than it shifts the column. The literal is cut off after the single quote,
	// which is the violation.
	v := lex_violation('{"é": \'')
	assert v.pos.line == 1
	// `{` `"` `é` `"` `:` `space` occupy columns 1 to 6, so the quote is column 7.
	assert v.pos.col == 7
	// The `é` is two bytes wide, so the quote is one byte further along than its
	// column: byte 7 rather than 6.
	assert v.pos.offset == 7
	assert v.pos.end_offset == 8
}

fn test_lex_positions_follow_a_comment() {
	v := lex_violation("{\n  // note\n  'a': 1\n}")
	assert v.pos.line == 3
	assert v.pos.col == 3
	// `{` is byte 0, the first newline byte 1, `// note` bytes 4 to 10, the second
	// newline byte 11, and the two spaces that follow it bytes 12 and 13.
	assert v.pos.offset == 14
}
