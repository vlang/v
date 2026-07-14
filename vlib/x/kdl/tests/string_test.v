module main

import x.kdl

fn test_string_escaped_newline_fold() {
	doc := kdl.parse('v "a\\\nb"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'ab'
}

fn test_string_escape_double_quote() {
	doc := kdl.parse('v "a\\"b"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'a"b'
}

fn test_string_escape_backslash() {
	doc := kdl.parse('v "a\\\\b"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'a\\b'
}

fn test_string_escape_backspace() {
	doc := kdl.parse('v "a\\bb"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'a\bb'
}

fn test_string_escape_formfeed() {
	doc := kdl.parse('v "a\\fb"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'a\x0cb'
}

fn test_string_escape_space() {
	doc := kdl.parse('v "a\\sb"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'a b'
}

fn test_string_empty() {
	doc := kdl.parse('v ""')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == ''
}

fn test_raw_string_single_hash() {
	doc := kdl.parse('v #"hello"#')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_raw_string_no_escape_processing() {
	doc := kdl.parse('v #"\\n\\t\\r"#')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == '\\n\\t\\r'
}

fn test_raw_string_contains_quotes() {
	doc := kdl.parse('v #"hello \\"world\\""#')!
	assert kdl.as_string(doc.nodes[0].entries[0].value).contains('"')
}

fn test_multiline_basic() {
	src := 'md """
  hello
  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_multiline_with_indent() {
	src := 'md """
    hello
    """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_multiline_multiple_lines() {
	src := 'md """
  line1
  line2
  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'line1\nline2'
}

fn test_multiline_empty() {
	src := 'md """
"""'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == ''
}

fn test_multiline_single_line_rejected() {
	doc := kdl.parse('md """hello"""') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_multiline_raw_basic() {
	src := 'md #"""
  hello
  """#'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_multiline_raw_no_escapes() {
	src := 'md #"""
    \\n\\t\\r
    """#'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == '\\n\\t\\r'
}

fn test_quote_string_basic() {
	assert kdl.quote_string('hello') == '"hello"'
}

fn test_quote_string_with_quote() {
	assert kdl.quote_string('he"llo') == '"he\\"llo"'
}

fn test_quote_string_with_backslash() {
	assert kdl.quote_string('he\\llo') == '"he\\\\llo"'
}

fn test_quote_string_with_tab() {
	assert kdl.quote_string('he\tllo') == '"he\\tllo"'
}

fn test_quote_string_with_newline() {
	assert kdl.quote_string('he\nllo') == '"he\\nllo"'
}

fn test_unquote_string_basic() {
	assert kdl.unquote_string('"hello"')! == 'hello'
}

fn test_unquote_string_empty() {
	assert kdl.unquote_string('""')! == ''
}

fn test_unquote_string_invalid() {
	assert kdl.unquote_string('hello') or { 'error' } == 'error'
}

fn test_raw_string_fn_basic() {
	assert kdl.raw_string('test') == '#"test"#'
}

fn test_raw_string_fn_with_quote() {
	got := kdl.raw_string('test "quote"')
	assert got.starts_with('#')
	assert got.ends_with('#')
	assert got.contains('test "quote"')
	assert got.ends_with('"#')
}

fn test_can_be_bare_identifier_true() {
	assert kdl.can_be_bare_identifier('hello') == true
	assert kdl.can_be_bare_identifier('hello-world') == true
	assert kdl.can_be_bare_identifier('foo123') == true
}

fn test_can_be_bare_identifier_false() {
	assert kdl.can_be_bare_identifier('true') == false
	assert kdl.can_be_bare_identifier('null') == false
	assert kdl.can_be_bare_identifier('') == false
	assert kdl.can_be_bare_identifier('hello world') == false
}

fn test_multiline_raw_followed_by_node() {
	src := 'md #"""
  hello
  """#
next-node'
	doc := kdl.parse(src)!
	assert doc.nodes.len == 2
	assert doc.nodes[0].name == 'md'
	assert doc.nodes[0].entries.len == 1
	assert doc.nodes[1].name == 'next-node'
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_invalid_escape_error() {
	doc := kdl.parse('v "\\x"') or { kdl.Document{} }
	assert doc.nodes.len == 0
	doc2 := kdl.parse('v "\\y"') or { kdl.Document{} }
	assert doc2.nodes.len == 0
}

fn test_quote_string_escapes_disallowed_literals() {
	q := kdl.quote_string('\x7f')
	assert q.len > 4
	assert q[0] == u32(34)
	assert q[q.len - 1] == u32(34)

	q3 := kdl.quote_string('\x01')
	assert q3.len > 4
	assert q3[0] == u32(34)
}

fn test_unquote_string_rejects_unknown_escape() {
	kdl.unquote_string('"\\x"') or {
		assert err.msg().contains('invalid escape')
		return
	}
	assert false, 'should have errored on unknown escape'
}

fn test_unquote_string_fold_newline_works() {
	result := kdl.unquote_string('"\\\n"')!
	assert result == ''
}

fn test_string_escape_carriage_return() {
	doc := kdl.parse('v "a\\rb"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'a\rb'
}

fn test_raw_string_hash_quote_inside_double_hash() {
	doc := kdl.parse('v ##"has "#" inside"##')!
	assert kdl.as_string(doc.nodes[0].entries[0].value).contains('#')
}

fn test_multiline_shorter_last_indent() {
	src := 'md """\n      foo\n  This is base\n          bar\n  """'
	doc := kdl.parse(src)!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val.contains('foo')
	assert val.contains('This is base')
	assert val.contains('bar')
}

fn test_multiline_empty_lines_preserved() {
	src := 'md """\n  line1\n\n  line2\n  """'
	doc := kdl.parse(src)!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val.contains('line1\n\nline2')
}

fn test_multiline_ws_escape_join_then_dedent() {
	src := 'md """\n  foo \\\n  bar\n  """'
	doc := kdl.parse(src)!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val.contains('foo')
	assert val.contains('bar')
}

fn test_raw_string_partial_hash_no_close() {
	doc := kdl.parse('v ###"has "## inside"###')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'has "## inside'
}

fn test_raw_string_hash_quote_edge() {
	doc := kdl.parse('v ##"value"##')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'value'
}

fn test_multiline_raw_contains_literal_escapes() {
	src := 'md #"""\n  \\n\\t\\\\\n  """#'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == '\\n\\t\\\\'
}

fn test_unicode_escape_null() {
	doc := kdl.parse('v "\\u{0}"')!
	s := kdl.as_string(doc.nodes[0].entries[0].value)
	assert s.len == 1 && s[0] == 0x00
}

fn test_unicode_escape_one_hex_digit() {
	doc := kdl.parse('v "\\u{a}"')!
	s := kdl.as_string(doc.nodes[0].entries[0].value)
	assert s.len == 1 && s[0] == 0x0a
}

fn test_unicode_escape_six_hex_digits() {
	doc := kdl.parse('v "\\u{10FFFF}"')!
	s := kdl.as_string(doc.nodes[0].entries[0].value)
	assert s.len == 4
}

fn test_trailing_quote_after_ident_rejected() {
	doc := kdl.parse('v "a"b"') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_multiline_raw_empty() {
	src := 'md #"""\n"""#'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == ''
}

fn test_raw_string_hash_mismatch_rejected() {
	doc := kdl.parse('v ##"value"#') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_backslash_at_eof_in_string_rejected() {
	data := [u8(118), 32, 34, 97, 92]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_bidi_control_literal_in_quoted_string_rejected() {
	data := [u8(118), 32, 34, 0xE2, 0x80, 0x8E, 34]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_underscore_bare_node_name() {
	doc := kdl.parse('_')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == '_'
}

fn test_emoji_single_char_node_name() {
	data := [u8(0xF0), 0x9F, 0x94, 0xA5, 32, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name.len > 0
	assert doc.nodes[0].entries.len == 1
}

fn test_empty_quoted_property_key() {
	doc := kdl.parse('node ""=val')!
	assert kdl.property_exists(&doc.nodes[0], '')
	if val := kdl.property_get(&doc.nodes[0], '') {
		assert kdl.as_string(val) == 'val'
	} else {
		assert false
	}
}

// Per KDL 2.0 formal grammar §4, node-space and node-terminators
// (including single-line comments) are valid after the closing """ of
// a multiline string on the same line.
fn test_multiline_trailing_comment_on_close_line() {
	src := 'md """
  hello
  """ // trailing comment'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_multiline_raw_trailing_comment_on_close_line() {
	src := 'md #"""
  hello
  """# // trailing comment'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_multiline_raw_trailing_whitespace_on_close_line() {
	src := 'md #"""
  hello
  """#   '
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_multiline_raw_with_comment_not_mistaken_for_content() {
	// Verify that a comment after the close marker doesn't cause
	// the scanner to continue looking for a closer in subsequent lines.
	src := 'md #"""
  hello
  """# // this is a comment, not content
extra-node'
	doc := kdl.parse(src)!
	assert doc.nodes.len == 2
	assert doc.nodes[0].name == 'md'
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
	assert doc.nodes[1].name == 'extra-node'
}

fn test_multiline_raw_double_slash_not_content() {
	// """# // x should close the raw multiline, not include // as content,
	// and a following node should parse correctly.
	src := 'md #"""
  hello
  """# // comment
world'
	doc := kdl.parse(src)!
	assert doc.nodes.len == 2
	assert doc.nodes[0].name == 'md'
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
	assert doc.nodes[1].name == 'world'
}
