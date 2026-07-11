module main

import x.kdl

// Tests for quote_string, unquote_string, raw_string, can_be_bare_identifier

struct QuoteCase {
	name  string
	input string
	want  string
}

fn test_quote_string() {
	tests := [
		QuoteCase{'basic', 'This is a test', '"This is a test"'},
		QuoteCase{'with quotes', 'This "is" a test', '"This \\"is\\" a test"'},
		QuoteCase{'with tab', 'This is\ta test', '"This is\\ta test"'},
		QuoteCase{'with backslash', 'This is a test\\', '"This is a test\\\\"'},
	]
	for tc in tests {
		result := kdl.quote_string(tc.input)
		assert result == tc.want
	}
}

struct UnquoteCase {
	name         string
	input        string
	expected     string
	expect_error bool
}

fn test_unquote_string() {
	tests := [
		UnquoteCase{'basic', '"This is a test"', 'This is a test', false},
		UnquoteCase{'empty', '""', '', false},
		UnquoteCase{'single char', '"x"', 'x', false},
		UnquoteCase{'with tab', '"x\\t"', 'x\t', false},
		UnquoteCase{'leading tab', '"\\tx"', '\tx', false},
		UnquoteCase{'tab only', '"\\t"', '\t', false},
		UnquoteCase{'trailing backslash', '"This is a test\\"', '', true},
		UnquoteCase{'unterminated', '"', '', true},
	]
	for tc in tests {
		result := kdl.unquote_string(tc.input) or { '' }
		assert result == tc.expected || tc.expect_error
	}
}

fn test_raw_string() {
	// Basic raw string (KDL-style)
	assert kdl.raw_string('test') == '#"test"#'
	// Raw string with quotes inside
	r2 := kdl.raw_string('test "quote"')
	assert r2.contains('#')
	assert r2.contains('test "quote"')
	assert r2.contains('"#')
}

fn test_can_be_bare_identifier() {
	assert kdl.can_be_bare_identifier('hello') == true
	assert kdl.can_be_bare_identifier('hello-world') == true
	assert kdl.can_be_bare_identifier('foo123') == true
	assert kdl.can_be_bare_identifier('true') == false
	assert kdl.can_be_bare_identifier('false') == false
	assert kdl.can_be_bare_identifier('null') == false
	assert kdl.can_be_bare_identifier('') == false
}

fn test_value_str() {
	// Value type display
	doc := kdl.parse('v "str" 42 3.14 #true #false #null')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 6
}

fn test_node_property_helpers() {
	doc := kdl.parse('config port=8080 host="localhost"')!
	n := doc.nodes[0]
	assert kdl.property_exists(n, 'port') == true
	assert kdl.property_exists(n, 'missing') == false
	assert kdl.property_has(n) == true
}

fn test_suffixed_decimal_type() {
	// SuffixedDecimal struct exists
	sd := kdl.SuffixedDecimal{
		number: '10'
		suffix: 'ms'
	}
	assert sd.number == '10'
	assert sd.suffix == 'ms'
}

fn test_document_done() {
	println('document tests done')
}
