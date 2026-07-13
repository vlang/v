module main

import x.kdl

fn assert_parse_fails(src string) {
	kdl.parse(src) or { return }
	assert false, 'expected parse error for ${src}'
}

fn assert_parse_bytes_fails(data []u8) {
	kdl.parse(data.bytestr()) or { return }
	assert false, 'expected parse error'
}

fn test_raw_string_u0001_rejected() {
	data := [u8(118), 32, 35, 34, 0x01, 34, 35]
	assert_parse_bytes_fails(data)
}

fn test_raw_string_u007f_rejected() {
	data := [u8(118), 32, 35, 34, 0x7f, 34, 35]
	assert_parse_bytes_fails(data)
}

fn test_raw_string_u0080_valid() {
	data := [u8(118), 32, 35, 34, 0xc2, 0x80, 34, 35]
	doc := kdl.parse(data.bytestr())!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val.len == 2
	assert val[0] == 0xc2
	assert val[1] == 0x80
}

fn test_raw_string_valid_still_works() {
	doc := kdl.parse('v #"hello"#')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_quoted_string_del_rejected() {
	data := [u8(118), 32, 34, 0x7f, 34]
	assert_parse_bytes_fails(data)
}

fn test_quoted_string_u0080_valid() {
	data := [u8(118), 32, 34, 0xc2, 0x80, 34]
	doc := kdl.parse(data.bytestr())!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val.len == 2
	assert val[0] == 0xc2
	assert val[1] == 0x80
}

fn test_quoted_string_del_escape_works() {
	doc := kdl.parse('v "\\u{7f}"')!
	s := kdl.as_string(doc.nodes[0].entries[0].value)
	assert s.len == 1 && s[0] == 0x7f
}

fn test_quoted_string_u001f_rejected() {
	data := [u8(118), 32, 34, 0x1f, 34]
	assert_parse_bytes_fails(data)
}

fn test_signed_leading_zero_negative_valid() {
	doc := kdl.parse('v -07')!
	assert doc.nodes.len == 1
	assert kdl.as_int(doc.nodes[0].entries[0].value) == -7
}

fn test_signed_leading_zero_positive_valid() {
	doc := kdl.parse('v +07')!
	assert doc.nodes.len == 1
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 7
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

fn test_escline_at_eof() {
	doc := kdl.parse('node \\')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
}

fn test_escline_normal() {
	doc := kdl.parse('node \\\n  arg1')!
	assert doc.nodes[0].entries.len == 1
}

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
	assert kdl.as_int(doc.nodes[0].entries[0].value) == -255
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

fn test_slashdash_node() {
	doc := kdl.parse('/- commented\nreal "val"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'real'
}

fn test_slashdash_entry() {
	doc := kdl.parse('node /- 1 2')!
	assert doc.nodes[0].entries.len == 1
}

fn test_unicode_u2028_line_separator_splits_nodes() {
	doc := kdl.parse('node1\xe2\x80\xa8node2')!
	assert doc.nodes.len == 2
	assert doc.nodes[0].name == 'node1'
	assert doc.nodes[1].name == 'node2'
}

fn test_unicode_u2029_paragraph_separator_splits_nodes() {
	doc := kdl.parse('node1\xe2\x80\xa9node2')!
	assert doc.nodes.len == 2
}

fn test_unicode_u0085_nel_splits_nodes() {
	doc := kdl.parse('node1\xc2\x85node2')!
	assert doc.nodes.len == 2
}

fn test_relaxed_ident_breaks_on_nbsp() {
	mut relax := kdl.RelaxedNonCompliant{
		flags: kdl.nginx_syntax
	}
	opts := kdl.ParseOpts{
		relaxed: relax
	}
	doc := kdl.parse_opts('node\xc2\xa0test', opts)!
	assert doc.nodes[0].name == 'node'
}

fn test_escline_in_string_requires_newline() {
	doc := kdl.parse('v "a\\ b"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'ab'
}

fn test_hash_keyword_nbsp_delimits() {
	doc := kdl.parse('v #inf\xc2\xa0test')!
	assert doc.nodes[0].entries.len == 2
}

fn test_cr_alone_newline() {
	doc := kdl.parse('node1\rnode2')!
	assert doc.nodes.len == 2
}

fn test_number_like_idents_rejected() {
	assert_parse_fails('node 1.0v2')
	assert_parse_fails('node -1em')
}

fn test_uppercase_number_prefixes_rejected() {
	assert_parse_fails('v 0XFF')
	assert_parse_fails('v 0O77')
	assert_parse_fails('v 0B10')
}

fn test_property_value_type_annotation() {
	doc := kdl.parse('node key=(u8)123')!
	p := doc.nodes[0].entries[0] as kdl.Property
	assert p.type_name == 'u8'
	assert kdl.as_int(p.value) == 123
}

fn test_property_value_type_annotation_roundtrip() {
	doc := kdl.parse('node key=(u8)123')!
	out := kdl.format(doc)!
	assert out.contains('key=(u8)123')
	doc2 := kdl.parse(out)!
	p := doc2.nodes[0].entries[0] as kdl.Property
	assert p.key == 'key'
	assert p.type_name == 'u8'
	assert kdl.as_int(p.value) == 123
}

fn test_type_annotation_with_whitespace() {
	doc := kdl.parse('( u8 )node "val"')!
	assert doc.nodes[0].type_name == 'u8'
}

fn test_multiline_newline_normalization_crlf() {
	src := 'md """\r\n  hello\r\n  world\r\n  """'
	doc := kdl.parse(src)!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val == 'hello\nworld'
}

fn test_multiline_whitespace_only_line() {
	src := 'md """\n  a\n \t \n  b\n  """'
	doc := kdl.parse(src)!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val == 'a\n\nb'
}

fn test_multiline_raw_newline_normalization() {
	src := 'md #"""\r\n  hello\r\n  """#'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_version_marker_ignored() {
	doc := kdl.parse('/- kdl-version 2\nnode "val"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
}

fn test_bom_at_non_start_rejected() {
	data := [u8(110), 0xEF, 0xBB, 0xBF, 111, 100, 101]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_slashdash_children_before_real() {
	doc := kdl.parse('node /- { child1 } { child2 }')!
	assert doc.nodes[0].children.len == 1
	assert doc.nodes[0].children[0].name == 'child2'
}

fn test_slashdash_children_after_real() {
	doc := kdl.parse('node { child1 } /- { child2 }')!
	assert doc.nodes[0].children.len == 1
	assert doc.nodes[0].children[0].name == 'child1'
}

fn test_slashdash_children_multiple_before() {
	doc := kdl.parse('node /- { c1 } /- { c2 } { c3 }')!
	assert doc.nodes[0].children.len == 1
	assert doc.nodes[0].children[0].name == 'c3'
}

fn test_slashdash_children_standalone() {
	doc := kdl.parse('node /- { child1 }')!
	assert doc.nodes[0].children.len == 0
}

fn test_slashdash_property_value() {
	assert_parse_fails('node key=/- val')
}

fn test_trailing_semicolons_children() {
	doc := kdl.parse('node { child1; child2; }')!
	assert doc.nodes[0].children.len == 2
}

fn test_consecutive_semicolons() {
	doc := kdl.parse('a;;b')!
	assert doc.nodes.len == 2
}

fn test_scientific_underscore_number() {
	doc := kdl.parse('v 1_000e10')!
	val := kdl.as_f64(doc.nodes[0].entries[0].value)
	assert val > 1e12
}

fn test_positive_decimal_exp() {
	doc := kdl.parse('v 0e+5')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 0.0
}

fn test_hex_invalid_digits_rejected() {
	doc := kdl.parse('v 0xGG') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_octal_invalid_digits_rejected() {
	doc := kdl.parse('v 0o78') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_binary_invalid_digits_rejected() {
	doc := kdl.parse('v 0b12') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_float_trailing_characters_rejected() {
	doc := kdl.parse('v 1.0x') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_exponent_after_sign_no_digits_rejected() {
	doc := kdl.parse('v 1e+') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_identifier_plus_characters() {
	assert kdl.parse('+..')!.nodes[0].name == '+..'
	assert kdl.parse('+.foo')!.nodes[0].name == '+.foo'
	assert kdl.parse('+abc')!.nodes[0].entries.len == 0
}

fn test_escaped_bidi_in_quoted_string() {
	doc := kdl.parse('v "\\u{200E}"')!
	s := kdl.as_string(doc.nodes[0].entries[0].value)
	assert s.len == 3
}

fn test_multiline_crlf_crlf_normalization() {
	src := 'md """\r\n\r\n  hello\r\n  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == '\nhello'
}

fn test_crlf_between_top_level_nodes() {
	doc := kdl.parse('node1\r\nnode2')!
	assert doc.nodes.len == 2
	assert doc.nodes[0].name == 'node1'
	assert doc.nodes[1].name == 'node2'
}

fn test_nel_between_top_level_nodes() {
	doc := kdl.parse('node1\xc2\x85node2')!
	assert doc.nodes.len == 2
}

fn test_vt_splits_nodes() {
	doc := kdl.parse('node1\x0bnode2')!
	assert doc.nodes.len == 2
	assert doc.nodes[0].name == 'node1'
	assert doc.nodes[1].name == 'node2'
}

fn test_ff_splits_nodes() {
	doc := kdl.parse('node1\x0cnode2')!
	assert doc.nodes.len == 2
}

fn test_vt_newline_in_multiline_string() {
	src := 'md """\x0b  hello\x0b  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_unicode_escape_max_codepoint() {
	doc := kdl.parse('v "\\u{10FFFF}"')!
	s := kdl.as_string(doc.nodes[0].entries[0].value)
	assert s.len == 4 // UTF-8: F4 8F BF BF
}

fn test_unicode_escape_above_max_rejected() {
	doc := kdl.parse('v "\\u{110000}"') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_multiline_crlf_crlf_is_single_blank_line() {
	src := 'md """\r\n\r\n  hello\r\n  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == '\nhello'
}

fn test_multiline_triple_crlf_is_two_blank_lines() {
	src := 'md """\r\n\r\n\r\n  hello\r\n  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == '\n\nhello'
}

fn test_multiline_indent_must_match_codepoints_exactly() {
	src := 'md """\n\t space\n \t tab\n\t """'
	kdl.parse(src) or {
		assert err.msg().contains('line indent must match')
		return
	}
	assert false, 'should have errored on indent mismatch'
}

fn test_single_trailing_underscore_decimal() {
	doc := kdl.parse('v 1_')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 1
}

fn test_underscore_in_float_fraction() {
	doc := kdl.parse('v 1_000.000_1')!
	val := kdl.as_f64(doc.nodes[0].entries[0].value)
	assert val > 1000.0 && val < 1001.0
}

fn test_underscore_in_scientific_exponent() {
	doc := kdl.parse('v 1.0e1_0')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 1.0e10
}

fn test_ident_plus_alone() {
	doc := kdl.parse('+')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == '+'
	assert doc.nodes[0].entries.len == 0
}

fn test_raw_string_node_name() {
	doc := kdl.parse('#"my node"# "val"')!
	assert doc.nodes[0].name == 'my node'
	assert doc.nodes[0].entries.len == 1
}

fn test_raw_string_node_name_with_type_annotation() {
	doc := kdl.parse('(person)#"full name"# "Bob"')!
	assert doc.nodes[0].type_name == 'person'
	assert doc.nodes[0].name == 'full name'
}

fn test_quoted_node_name_with_escape() {
	data := [u8(34), 109, 121, 92, 34, 110, 111, 100, 101, 34, 32, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'my"node'
}

fn test_quoted_node_name_with_tab() {
	doc := kdl.parse('"my\tname" "val"')!
	assert doc.nodes[0].name == 'my\tname'
}

fn test_raw_string_property_key() {
	doc := kdl.parse('node #"my key"#=42')!
	assert kdl.property_exists(&doc.nodes[0], 'my key')
	if val := kdl.property_get(&doc.nodes[0], 'my key') {
		assert kdl.as_int(val) == 42
	} else {
		assert false
	}
}

fn test_quoted_property_key() {
	doc := kdl.parse('node "my key"="val"')!
	assert kdl.property_exists(&doc.nodes[0], 'my key')
	if val := kdl.property_get(&doc.nodes[0], 'my key') {
		assert kdl.as_string(val) == 'val'
	} else {
		assert false
	}
}

fn test_ident_cjk_chars() {
	doc := kdl.parse('名前 "value"')!
	assert doc.nodes[0].name == '名前'
}

fn test_ident_accented() {
	doc := kdl.parse('café "val"')!
	assert doc.nodes[0].name == 'café'
}

fn test_ident_starting_with_digit_rejected() {
	doc := kdl.parse('5foo') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_bidi_control_in_ident_rejected() {
	data := [u8(110), 0xE2, 0x80, 0x8E, 111, 100, 101]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_bidi_control_u202a_in_ident_rejected() {
	data := [u8(110), 0xE2, 0x80, 0xAA, 111, 100, 101]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_bidi_control_u2066_in_ident_rejected() {
	data := [u8(110), 0xE2, 0x81, 0xA6, 111, 100, 101]
	doc := kdl.parse(data.bytestr()) or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_empty_type_annotation_rejected() {
	assert_parse_fails('()node "val"')
}

fn test_double_slashdash_before_node() {
	kdl.parse('/- /- node "val"') or {
		assert err.msg().contains('slashdash may not be followed')
		return
	}
	assert false, 'should have errored on consecutive slashdashes'
}

fn test_line_continuation_in_children() {
	doc := kdl.parse('parent {\n  child \\\n  "val"\n}')!
	assert doc.nodes[0].children.len == 1
	assert doc.nodes[0].children[0].name == 'child'
}

fn test_line_continuation_before_property() {
	doc := kdl.parse('node \\\n  key="val"')!
	assert kdl.property_exists(&doc.nodes[0], 'key')
}

fn test_line_continuation_with_nested_multiline_comment() {
	doc := kdl.parse('node \\ /* outer /* inner */ still */\n  arg1')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
	assert doc.nodes[0].entries.len == 1
}

fn test_zero_in_non_decimal_bases() {
	assert kdl.as_int(kdl.parse('v 0x0')!.nodes[0].entries[0].value) == 0
	assert kdl.as_int(kdl.parse('v 0o0')!.nodes[0].entries[0].value) == 0
	assert kdl.as_int(kdl.parse('v 0b0')!.nodes[0].entries[0].value) == 0
}

fn test_negative_zero_in_non_decimal_bases() {
	assert kdl.as_int(kdl.parse('v -0x0')!.nodes[0].entries[0].value) == 0
	assert kdl.as_int(kdl.parse('v -0o0')!.nodes[0].entries[0].value) == 0
	assert kdl.as_int(kdl.parse('v -0b0')!.nodes[0].entries[0].value) == 0
}

fn test_zero_float() {
	doc := kdl.parse('v 0.0')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 0.0
}

fn test_decimal_point_no_fraction_digits_rejected() {
	doc := kdl.parse('v 1.e10') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_underscore_in_fraction_between_digits() {
	doc := kdl.parse('v 1.0_0')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 1.0
}

fn test_consecutive_underscores_in_fraction() {
	doc := kdl.parse('v 1.0__1')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) > 1.0
	assert kdl.as_f64(doc.nodes[0].entries[0].value) < 1.02
}

fn test_multiple_decimal_points_rejected() {
	doc := kdl.parse('v 1.2.3') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_duplicate_property_keys_different_types() {
	doc := kdl.parse('node a=1 a=#true a="str"')!
	if val := kdl.property_get(&doc.nodes[0], 'a') {
		assert kdl.as_string(val) == 'str'
	} else {
		assert false
	}
}

fn test_unicode_ogham_space_whitespace() {
	data := [u8(110), 111, 100, 101, 0xE1, 0x9A, 0x80, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
	assert doc.nodes[0].entries.len == 1
}

fn test_unicode_en_space_whitespace() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x80, 0x82, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
}

fn test_surrogate_literal_in_quoted_string_rejected() {
	data := [u8(118), 32, 34, 0xED, 0xA0, 0x80, 34]
	assert_parse_bytes_fails(data)
}

fn test_unicode_escape_empty_braces_rejected() {
	assert_parse_fails('v "\\u{}"')
}

fn test_unicode_escape_leading_zero_surrogate_rejected() {
	assert_parse_fails('v "\\u{0000D800}"')
}

fn test_nel_newline_in_multiline_string() {
	src := 'md """\xc2\x85  hello\xc2\x85  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_ls_newline_in_multiline_string() {
	src := 'md """\xe2\x80\xa8  hello\xe2\x80\xa8  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_ps_newline_in_multiline_string() {
	src := 'md """\xe2\x80\xa9  hello\xe2\x80\xa9  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_unterminated_block_comment_rejected() {
	assert_parse_fails('node /* unclosed')
}

fn test_bom_and_version_marker() {
	mut data := [u8(0xEF), 0xBB, 0xBF, 47, 45, 32, 107, 100, 108, 45, 118, 101, 114, 115, 105,
		111, 110, 32, 50, 10, 110, 111, 100, 101]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
}

fn test_version_marker_kdl1() {
	doc := kdl.parse('/- kdl-version 1\nnode "val"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
}

fn test_slashdash_node_with_type_annotation() {
	doc := kdl.parse('/- (person)node\nreal "val"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'real'
}

fn test_type_annotation_raw_newline_rejected() {
	data := [u8(40), 10, 41, 110, 111, 100, 101, 32, 34, 118, 97, 108, 34]
	assert_parse_bytes_fails(data)
}

fn test_escline_with_multiline_comment() {
	doc := kdl.parse('node \\ /* block */\n  arg1')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
	assert doc.nodes[0].entries.len == 1
}

fn test_deeply_nested_children_5_levels() {
	doc := kdl.parse('a{b{c{d{e{f}}}}}')!
	mut node := doc.nodes[0]
	mut depth := 0
	for node.children.len > 0 {
		depth++
		node = node.children[0]
	}
	assert depth == 5
	assert node.name == 'f'
}

fn test_reserved_type_annotations_no_crash() {
	_ := kdl.parse('node (date-time)"2024-01-01"')!
	_ := kdl.parse('node (uuid)"550e8400-e29b-41d4-a716-446655440000"')!
	_ := kdl.parse('node (ipv4)"192.168.1.1"')!
	_ := kdl.parse('node (url)"https://example.com"')!
	_ := kdl.parse('node (email)"user@example.com"')!
	_ := kdl.parse('(published)date "2024-01-01"')!
	assert true
}

fn test_hash_true_mixed_case_rejected() {
	assert_parse_fails('v #True')
	assert_parse_fails('v #TRUE')
}

fn test_hash_null_mixed_case_rejected() {
	assert_parse_fails('v #Null')
	assert_parse_fails('v #NULL')
}

fn test_raw_string_double_hash_with_triple_quote_inside() {
	doc := kdl.parse('v ##" has """ inside "##')!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val.contains('"""')
}

fn test_raw_string_many_hashes() {
	doc := kdl.parse('v ##########"hello"##########')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_mixed_newline_types_in_document() {
	mut data := [u8(110), 111, 100, 101, 49, 13, 110, 111, 100, 101, 50, 0xE2, 0x80, 0xA8, 110,
		111, 100, 101, 51]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes.len == 3
	assert doc.nodes[0].name == 'node1'
	assert doc.nodes[1].name == 'node2'
	assert doc.nodes[2].name == 'node3'
}

fn test_hair_space_whitespace() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x80, 0x8A, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
	assert doc.nodes[0].entries.len == 1
}

fn test_deeply_nested_children_10_levels() {
	src := 'a{b{c{d{e{f{g{h{i{j{k}}}}}}}}}}'
	doc := kdl.parse(src)!
	mut node := doc.nodes[0]
	mut depth := 0
	for node.children.len > 0 {
		depth++
		node = node.children[0]
	}
	assert depth == 10
	assert node.name == 'k'
}

fn test_slashdash_comment_between_name_and_brace() {
	doc := kdl.parse('node /- { ignored } { real }')!
	assert doc.nodes[0].children.len == 1
	assert doc.nodes[0].children[0].name == 'real'
}

fn test_consecutive_slashdash_entries() {
	doc := kdl.parse('node /- 1 /- key=val /- 2 3')!
	assert doc.nodes[0].entries.len == 1
}

fn test_slashdash_entry_before_another_slashdash_entry() {
	doc := kdl.parse('node /- arg /- 1 2')!
	assert doc.nodes[0].entries.len == 1
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 2
}

fn test_document_only_slashdash_nodes() {
	doc := kdl.parse('/- a\n/- b\n/- c')!
	assert doc.nodes.len == 0
}

fn test_multiline_indent_unicode_nbsp_vs_space() {
	src := 'md """\n\tmatched\n not_matched\n\t"""'
	kdl.parse(src) or {
		assert err.msg().contains('line indent must match')
		return
	}
	assert false, 'should have errored on indent mismatch'
}

fn test_narrow_nbsp_whitespace() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x80, 0xAF, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
	assert doc.nodes[0].entries.len == 1
}

fn test_medium_math_space_whitespace() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x81, 0x9F, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
	assert doc.nodes[0].entries.len == 1
}

fn test_c0_null_literal_in_document_rejected() {
	data := [u8(110), 111, 100, 101, 0x00, 34, 118, 97, 108, 34]
	assert_parse_bytes_fails(data)
}

fn test_slashdash_before_typed_property_key_rejected() {
	assert_parse_fails('node a=1 /- (u8)b=2 c=3')
}

fn test_properties_preserve_argument_order() {
	doc := kdl.parse('node 1 key=val 2')!
	assert doc.nodes[0].entries.len == 3
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 1
	assert kdl.as_int(doc.nodes[0].entries[2].value) == 2
}

fn test_consecutive_underscores_in_float_fraction_triple() {
	doc := kdl.parse('v 1.0___2')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) > 1.0
	assert kdl.as_f64(doc.nodes[0].entries[0].value) < 1.03
}

fn test_consecutive_underscores_in_float_exponent_2() {
	doc := kdl.parse('v 1.0e1__0')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 1.0e10
}

fn test_underscore_after_decimal_point_rejected_2() {
	assert_parse_fails('v 1._0')
	assert_parse_fails('v 0._1')
}

fn test_underscore_after_exponent_sign_rejected() {
	assert_parse_fails('v 1e+_1')
	assert_parse_fails('v 1e-_1')
}

fn test_multiline_whitespace_only_line_2() {
	src := 'md """\n  a\n \t \n  b\n  """'
	doc := kdl.parse(src)!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val == 'a\n\nb'
}

fn test_multiline_raw_whitespace_only_line() {
	src := 'md #"""\n  a\n \t \n  b\n  """#'
	doc := kdl.parse(src)!
	val := kdl.as_string(doc.nodes[0].entries[0].value)
	assert val == 'a\n\nb'
}

fn test_multiline_escaped_whitespace_not_empty() {
	src := 'md """\n\t\\s\n\t"""'
	doc := kdl.parse(src)!
	assert doc.nodes.len == 1
}

fn test_unicode_escape_seven_hex_digits_rejected() {
	assert_parse_fails('v "\\u{0000001}"')
}

fn test_unicode_escape_invalid_hex_rejected() {
	assert_parse_fails('v "\\u{GHIJ}"')
}

fn test_trailing_underscore_float() {
	doc := kdl.parse('v 1.0_')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 1.0
	doc2 := kdl.parse('v 100_')!
	assert kdl.as_int(doc2.nodes[0].entries[0].value) == 100
}

fn test_underscore_before_exponent() {
	doc := kdl.parse('v 1_e10')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 1e10
}

fn test_quoted_ws_escape_spaces_before_newline() {
	data := [u8(118), 32, 34, 72, 101, 108, 108, 111, 92, 32, 32, 32, 10, 87, 111, 114, 108, 100,
		34]
	doc := kdl.parse(data.bytestr())!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'HelloWorld'
}

fn test_multiline_empty_with_spaces_on_closing_line() {
	src := 'md """\n  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == ''
}

fn test_multiline_crlf_crlf_normalized_to_two_lf() {
	src := 'md """\r\n\r\n  hello\r\n  """'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == '\nhello'
}

fn test_multiline_tab_indent_matching() {
	src := 'md """\n\thello\n\tworld\n\t"""'
	doc := kdl.parse(src)!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello\nworld'
}

fn test_multiline_tab_indent_mismatch() {
	src := 'md """\n\thello\n """'
	kdl.parse(src) or {
		assert err.msg().contains('line indent must match')
		return
	}
	assert false, 'should have errored on indent mismatch'
}

fn test_type_annotation_before_property_key_rejected() {
	assert_parse_fails('node (u8)key=123')
}

fn test_strict_colon_is_identifier_character() {
	doc := kdl.parse('node key:value')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 1
	arg := doc.nodes[0].entries[0] as kdl.Argument
	assert kdl.as_string(arg.value) == 'key:value'
}

fn test_identifier_exclusion_list_ascii_punctuation() {
	for src in ['@name', 'a@b', 'a|b', "a'b", 'a`b', ':name', 'a:b'] {
		doc := kdl.parse(src)!
		assert doc.nodes.len == 1
		assert doc.nodes[0].name == src
		assert kdl.can_be_bare_identifier(src)
	}
}

fn test_signed_and_dotted_identifier_helper_boundaries() {
	for src in ['+', '+.', '+.foo', '-', '-.foo', '.', '..'] {
		assert kdl.can_be_bare_identifier(src)
	}
	assert !kdl.can_be_bare_identifier('+.5')
	assert !kdl.can_be_bare_identifier('-.5')
	assert !kdl.can_be_bare_identifier('.5')
}

fn test_identifier_helper_rejects_unicode_space_and_disallowed_literals() {
	assert !kdl.can_be_bare_identifier('a\xc2\xa0b')
	assert !kdl.can_be_bare_identifier('a\xe2\x80\xa8b')
	assert !kdl.can_be_bare_identifier('a\xe2\x80\x8eb')
	assert !kdl.can_be_bare_identifier('a\x7fb')
}

fn test_slashdash_entry_allows_line_space_before_argument() {
	doc := kdl.parse('node /-\n  1 2')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 1
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 2
}

fn test_entries_rejected_after_slashdashed_children_block() {
	assert_parse_fails('node /- { ignored } arg')
	assert_parse_fails('node { real } /- arg')
}

fn test_slashdash_requires_valid_target() {
	assert_parse_fails('/-')
	assert_parse_fails('node /-')
	assert_parse_fails('node /- ;')
	assert_parse_fails('/- { child }')
}

fn test_slashdashed_children_block_requires_closing_brace() {
	assert_parse_fails('node /- { child')
	assert_parse_fails('/- node { child')
}

fn test_quoted_string_rejects_slash_escape() {
	assert_parse_fails('node "a\\/b"')
	assert_parse_fails('node "a\\/* comment */b"')
}

fn test_type_annotation_with_quoted_string_roundtrips_through_format() {
	doc := kdl.parse('("foo bar")node "val"')!
	out := kdl.format(doc)!
	assert out.starts_with('("foo bar")node')
	doc2 := kdl.parse(out)!
	assert doc2.nodes[0].type_name == 'foo bar'
}

fn test_quoted_u0080_roundtrips_as_utf8_codepoint() {
	data := [u8(110), 111, 100, 101, 32, 34, 0xc2, 0x80, 32, 120, 34]
	doc := kdl.parse(data.bytestr())!
	out := kdl.format(doc)!
	doc2 := kdl.parse(out)!
	s := kdl.as_string(doc2.nodes[0].entries[0].value)
	assert s.len == 4
	assert s[0] == 0xc2
	assert s[1] == 0x80
	assert s[2] == 32
	assert s[3] == 120
}

fn test_quote_string_keeps_u0080_and_escapes_u0085_newline() {
	u0080 := [u8(0xc2), 0x80].bytestr()
	q0080 := kdl.quote_string(u0080)
	assert q0080.len == 4
	assert q0080[0] == 34
	assert q0080[1] == 0xc2
	assert q0080[2] == 0x80
	assert q0080[3] == 34

	u0085 := [u8(0xc2), 0x85].bytestr()
	q0085 := kdl.quote_string(u0085)
	assert q0085 == '"\\u{85}"'
	doc := kdl.parse('node ' + q0085)!
	s := kdl.as_string(doc.nodes[0].entries[0].value)
	assert s.len == 2
	assert s[0] == 0xc2
	assert s[1] == 0x85
}

fn test_raw_string_u0000_rejected() {
	data := [u8(118), 32, 35, 34, 0x00, 34, 35]
	assert_parse_bytes_fails(data)
}

fn test_entries_require_node_space_separator() {
	assert_parse_fails('node"arg"')
	assert_parse_fails('node(u8)1')
	assert_parse_fails('node 1"arg"')
}

fn test_block_comment_with_newline_is_node_space() {
	doc := kdl.parse('node /*\n*/ arg')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 1
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'arg'
}

fn test_multiline_string_rejects_invalid_escapes() {
	assert_parse_fails('v """\n  a\\q\n  """')
	assert_parse_fails('v """\n  a\\u123\n  """')
}

fn test_multiline_dedent_before_regular_escapes() {
	assert_parse_fails('v """\n\\s\\sfoo\n  """')
	assert_parse_fails('v """\n\\tfoo\n\t"""')
	doc := kdl.parse('v """\n  \\sfoo\n  """')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == ' foo'
}

fn test_multiline_unicode_whitespace_indent() {
	doc := kdl.parse('v """\n\xc2\xa0hello\n\xc2\xa0"""')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
	raw_doc := kdl.parse('v #"""\n\xc2\xa0hello\n\xc2\xa0"""#')!
	assert kdl.as_string(raw_doc.nodes[0].entries[0].value) == 'hello'
}

fn test_scientific_float_format_roundtrip_preserves_value() {
	doc := kdl.parse('v 1.23456789e10')!
	out := kdl.format(doc)!
	doc2 := kdl.parse(out)!
	assert kdl.as_f64(doc2.nodes[0].entries[0].value) == kdl.as_f64(doc.nodes[0].entries[0].value)
}

fn test_negative_nan_is_valid_identifier_string() {
	doc := kdl.parse('node -nan')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 1
	assert kdl.as_string(doc.nodes[0].entries[0].value) == '-nan'
	out := kdl.format(doc)!
	assert out.contains('-nan')
}

fn test_integer_literals_reject_i64_overflow() {
	assert_parse_fails('v 9223372036854775808')
	assert_parse_fails('v -9223372036854775809')
	assert_parse_fails('v 0x8000000000000000')
	assert_parse_fails('v -0x8000000000000001')
	doc := kdl.parse('v -9223372036854775808 -0x8000000000000000')!
	assert kdl.as_i64(doc.nodes[0].entries[0].value) == -9223372036854775807 - 1
	assert kdl.as_i64(doc.nodes[0].entries[1].value) == -9223372036854775807 - 1
	min_bases :=
		kdl.parse('v -0x8000000000000000 -0b1000000000000000000000000000000000000000000000000000000000000000 -0o1000000000000000000000')!
	out := kdl.format(min_bases)!
	assert out.contains('-0x8000000000000000')
	assert out.contains('-0b1000000000000000000000000000000000000000000000000000000000000000')
	assert out.contains('-0o1000000000000000000000')
}

fn test_document_must_be_valid_utf8() {
	data := [u8(118), 32, 34, 0xff, 34]
	assert_parse_bytes_fails(data)
}

fn test_unquote_string_rejects_surrogate_escape() {
	kdl.unquote_string('"\\u{D800}"') or { return }
	assert false, 'should have rejected surrogate unicode escape'
}

fn test_reserved_type_annotations_more() {
	_ := kdl.parse('node (base64)"SGVsbG8="')!
	_ := kdl.parse('node (regex)".*"')!
	_ := kdl.parse('node (duration)"PT1H30M"')!
	_ := kdl.parse('node (currency)"USD"')!
	_ := kdl.parse('node (hostname)"example.com"')!
	_ := kdl.parse('node (decimal)"123.456"')!
	_ := kdl.parse('node (country-2)"US"')!
	_ := kdl.parse('node (country-3)"USA"')!
	_ := kdl.parse('node (url-template)"https://{host}/path"')!
	assert true
}

fn test_unicode_en_quad_whitespace() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x80, 0x80, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
	assert doc.nodes[0].entries.len == 1
}

fn test_unicode_em_quad_whitespace() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x80, 0x81, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_three_per_em_space() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x80, 0x84, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_four_per_em_space() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x80, 0x85, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_six_per_em_space() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x80, 0x86, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_punctuation_space() {
	data := [u8(110), 111, 100, 101, 0xE2, 0x80, 0x88, 34, 118, 97, 108, 34]
	doc := kdl.parse(data.bytestr())!
	assert doc.nodes[0].name == 'node'
}
