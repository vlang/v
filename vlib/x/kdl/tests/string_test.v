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
	assert kdl.raw_string('test') == 'r"test"'
}

fn test_raw_string_fn_with_quote() {
	got := kdl.raw_string('test "quote"')
	assert got.starts_with('r#')
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
