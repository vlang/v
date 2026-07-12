module main

import x.kdl

// Fix 1: Raw strings reject disallowed literal code points

fn test_raw_string_u0001_rejected() {
	data := [u8(118), 32, 35, 34, 0x01, 34, 35]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_raw_string_u007f_rejected() {
	data := [u8(118), 32, 35, 34, 0x7f, 34, 35]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_raw_string_c1_rejected() {
	data := [u8(118), 32, 35, 34, 0xc2, 0x80, 34, 35]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_raw_string_valid_still_works() {
	doc := kdl.parse('v #"hello"#')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

// Fix 2: Quoted string rejects DEL and C1 controls

fn test_quoted_string_del_rejected() {
	data := [u8(118), 32, 34, 0x7f, 34]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_quoted_string_c1_rejected() {
	data := [u8(118), 32, 34, 0xc2, 0x80, 34]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_quoted_string_del_escape_works() {
	doc := kdl.parse('v "\\u{7f}"')!
	s := kdl.as_string(doc.nodes[0].entries[0].value)
	assert s.len == 1 && s[0] == 0x7f
}

fn test_quoted_string_u001f_rejected() {
	data := [u8(118), 32, 34, 0x1f, 34]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

// Fix 3: Signed decimal leading zeros rejected

fn test_signed_leading_zero_negative_rejected() {
	doc := kdl.parse('v -07') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_signed_leading_zero_positive_rejected() {
	doc := kdl.parse('v +07') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_negative_zero_still_ok() {
	doc := kdl.parse('v -0')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 0
}

fn test_positive_zero_still_ok() {
	doc := kdl.parse('v +0')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 0
}

fn test_negative_zero_underscore_still_ok() {
	doc := kdl.parse('v -0_1')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == -1
}

// Fix 4: Trailing/consecutive underscores in numbers

fn test_trailing_underscore_decimal() {
	doc := kdl.parse('v 12____')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 12
}

fn test_consecutive_underscore_decimal() {
	doc := kdl.parse('v 1___2')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 12
}

fn test_trailing_underscore_hex() {
	doc := kdl.parse('v 0xFF_')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 255
}

fn test_consecutive_underscore_hex() {
	doc := kdl.parse('v 0xF__F')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 255
}

fn test_trailing_underscore_oct() {
	doc := kdl.parse('v 0o77_')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 63
}

fn test_trailing_underscore_bin() {
	doc := kdl.parse('v 0b10_')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 2
}

fn test_leading_underscore_hex_rejected() {
	doc := kdl.parse('v 0x_FF') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_leading_underscore_oct_rejected() {
	doc := kdl.parse('v 0o_77') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

// Fix 5: \ at EOF valid line continuation

fn test_escline_at_eof() {
	doc := kdl.parse('node \\')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
}

fn test_escline_normal() {
	doc := kdl.parse('node \\\n  arg1')!
	assert doc.nodes[0].entries.len == 1
}

// Fix 6: Multiline \r handling

fn test_multiline_crlf_works() {
	src := 'md """' + '\r\n' + '  hello' + '\r\n' + '  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_multiline_lf_works() {
	doc := kdl.parse('md """\n  hello\n  """')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_multiline_raw_crlf_works() {
	src := 'md #"""' + '\r\n' + '  hello' + '\r\n' + '  """#'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

// Additional edge cases

fn test_bom_at_start() {
	mut data := [u8(0xef), 0xbb, 0xbf, 110, 111, 100, 101]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_nbsp_whitespace() {
	mut data := [u8(110), 111, 100, 101, 0xc2, 0xa0, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
	assert doc.nodes[0].entries.len == 1
}

fn test_keyword_inf_roundtrip() {
	doc := kdl.parse('v #inf')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) > 1e300
	out := kdl.format(doc)!
	assert out.contains('#inf')
}

fn test_keyword_ninf_roundtrip() {
	doc := kdl.parse('v #-inf')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) < -1e300
}

fn test_keyword_nan_roundtrip() {
	doc := kdl.parse('v #nan')!
	f := kdl.as_f64(doc.nodes[0].entries[0].value)
	assert f != f
}

fn test_negative_hex_roundtrip() {
	doc := kdl.parse('v -0xFF')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == -255
	out := kdl.format(doc)!
	assert out.contains('-0x')
	doc2 := kdl.parse(out)!
	assert kdl.as_int(doc2.nodes[0].entries[0].value) == -255
}

fn test_escape_unicode_emoji() {
	doc := kdl.parse('v "\\u{1F600}"')!
	s := kdl.as_string(doc.nodes[0].entries[0].value)
	assert s.len == 4 && s[0] == 0xf0
}

fn test_escape_unicode_surrogate_rejected() {
	doc := kdl.parse('v "\\u{D800}"') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

// test_escape_unicode_no_braces_rejected: requires unbraced \\u fix (not yet applied)

fn test_slashdash_node() {
	doc := kdl.parse('/- commented\nreal "val"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'real'
}

fn test_slashdash_entry() {
	doc := kdl.parse('node /- 1 2')!
	assert doc.nodes[0].entries.len == 1
}
