module main

import x.kdl
import x.kdl.document

fn test_parse_single_node() {
	doc := kdl.parse('my-node')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'my-node'
	assert doc.nodes[0].entries.len == 0
}

fn test_parse_node_with_string_arg() {
	doc := kdl.parse('name "Alice"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'name'
	assert doc.nodes[0].entries.len == 1
}

fn test_parse_node_with_multiple_args() {
	doc := kdl.parse('nums 1 2 3 4')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'nums'
	assert doc.nodes[0].entries.len == 4
}

fn test_parse_node_with_property() {
	doc := kdl.parse('config port=8080')!
	assert doc.nodes[0].name == 'config'
	assert doc.nodes[0].entries.len == 1
	assert kdl.property_exists(&doc.nodes[0], 'port')
}

fn test_parse_node_with_multiple_properties() {
	doc := kdl.parse('config host="localhost" port=8080 debug=#true')!
	assert doc.nodes[0].entries.len == 3
}

fn test_parse_node_mixed_args_and_props() {
	doc := kdl.parse('node 1 key=val 2')!
	assert doc.nodes[0].entries.len == 3
}

fn test_value_quoted_string() {
	doc := kdl.parse('v "hello world"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello world'
}

fn test_value_bare_identifier() {
	doc := kdl.parse('v hello')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_value_decimal_integer() {
	doc := kdl.parse('v 42')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 42
}

fn test_value_negative_integer() {
	doc := kdl.parse('v -17')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == -17
}

fn test_value_positive_integer() {
	doc := kdl.parse('v +17')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 17
}

fn test_value_hex() {
	doc := kdl.parse('v 0xFF')!
	e := doc.nodes[0].entries[0]
	match e {
		document.Argument {
			match e.value {
				document.IntVal {
					assert e.value.value == 255
					assert e.value.flag == .hex
				}
				else {
					assert false
				}
			}
		}
		document.Property {
			assert false
		}
	}
}

fn test_value_float_decimal() {
	doc := kdl.parse('v 3.14')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) > 3.13
}

fn test_value_float_scientific() {
	doc := kdl.parse('v 1e10')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 1e10
}

fn test_value_float_scientific_neg_exp() {
	doc := kdl.parse('v 1.5e-3')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 0.0015
}

fn test_value_bool_true() {
	doc := kdl.parse('v #true')!
	assert kdl.as_bool(doc.nodes[0].entries[0].value) == true
}

fn test_value_bool_false() {
	doc := kdl.parse('v #false')!
	assert kdl.as_bool(doc.nodes[0].entries[0].value) == false
}

fn test_value_null() {
	doc := kdl.parse('v #null')!
	assert kdl.is_null(doc.nodes[0].entries[0].value) == true
}

fn test_keyword_number_inf_valid() {
	doc := kdl.parse('v #inf')!
	assert kdl.as_string(doc.nodes[0].entries[0].value).len > 0
}

fn test_keyword_number_ninf_valid() {
	doc := kdl.parse('v #-inf')!
	assert kdl.as_string(doc.nodes[0].entries[0].value).len > 0
}

fn test_keyword_number_nan_valid() {
	doc := kdl.parse('v #nan')!
	assert kdl.as_string(doc.nodes[0].entries[0].value).len > 0
}

fn test_bare_true_rejected() {
	doc := kdl.parse('node true') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_bare_false_rejected() {
	doc := kdl.parse('node false') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_bare_null_rejected() {
	doc := kdl.parse('node null') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_bare_inf_rejected() {
	doc := kdl.parse('node inf') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_bare_ninf_rejected() {
	doc := kdl.parse('node -inf') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_bare_nan_rejected() {
	doc := kdl.parse('node nan') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_children_empty_block() {
	doc := kdl.parse('node {}')!
	assert doc.nodes[0].children.len == 0
}

fn test_children_one_child() {
	doc := kdl.parse('parent { child "val" }')!
	assert doc.nodes[0].children.len == 1
	assert doc.nodes[0].children[0].name == 'child'
}

fn test_children_multiple() {
	doc := kdl.parse('parent {\n  child1 "a"\n  child2 "b"\n}')!
	assert doc.nodes[0].children.len == 2
}

fn test_children_nested() {
	src := 'a {\n  b {\n    c "deep"\n  }\n}'
	doc := kdl.parse(src)!
	assert doc.nodes[0].children[0].children[0].name == 'c'
}

fn test_children_with_semicolons() {
	doc := kdl.parse('parent { a "1"; b "2"; c "3" }')!
	assert doc.nodes[0].children.len == 3
}

fn test_semicolon_three_nodes() {
	doc := kdl.parse('a; b; c')!
	assert doc.nodes.len == 3
}

fn test_semicolon_in_children_block() {
	doc := kdl.parse('parent { a; b; c }')!
	assert doc.nodes[0].children.len == 3
}

fn test_comment_nested_block() {
	doc := kdl.parse('/* outer /* inner */ still */ node "val"')!
	assert doc.nodes[0].name == 'node'
}

fn test_comment_slashdash_arg() {
	doc := kdl.parse('node /- 1 2 3')!
	assert doc.nodes[0].entries.len == 2
}

fn test_comment_slashdash_property() {
	doc := kdl.parse('node a=1 /- b=2 c=3')!
	assert kdl.property_exists(&doc.nodes[0], 'a')
	assert !kdl.property_exists(&doc.nodes[0], 'b')
	assert kdl.property_exists(&doc.nodes[0], 'c')
}

fn test_comment_with_unicode() {
	doc := kdl.parse('\xef\xbb\xbf// \xe3\x82\xb3\xe3\x83\xa1\xe3\x83\xb3\xe3\x83\x88\nnode "val"')!
	assert doc.nodes[0].name == 'node'
}

fn test_line_continuation_basic() {
	doc := kdl.parse('node \\\n  arg1 arg2')!
	assert doc.nodes[0].entries.len == 2
}

fn test_line_continuation_with_comment() {
	doc := kdl.parse('node \\ // comment\n  arg1')!
	assert doc.nodes[0].entries.len == 1
}

fn test_type_annotation_on_node() {
	doc := kdl.parse('(person)node "name"')!
	assert doc.nodes[0].type_name == 'person'
	assert doc.nodes[0].name == 'node'
}

fn test_type_annotation_on_argument() {
	doc := kdl.parse('node (u8)123')!
	e := doc.nodes[0].entries[0]
	match e {
		document.Argument { assert e.type_name == 'u8' }
		document.Property {}
	}
}

fn test_error_unexpected_char() {
	doc := kdl.parse('@invalid') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_error_empty_document_no_error() {
	doc := kdl.parse('')!
	assert doc.nodes.len == 0
}

fn test_error_whitespace_only_no_error() {
	doc := kdl.parse('   \n  \n  ')!
	assert doc.nodes.len == 0
}

fn test_error_comments_only_no_error() {
	doc := kdl.parse('// comment\n/* block */')!
	assert doc.nodes.len == 0
}

fn test_multiple_top_level_nodes() {
	doc := kdl.parse('node1 "a"\nnode2 "b"\nnode3 "c"')!
	assert doc.nodes.len == 3
}

fn test_top_level_nodes_with_children() {
	doc := kdl.parse('a { b }\nc { d }')!
	assert doc.nodes.len == 2
	assert doc.nodes[0].children.len == 1
	assert doc.nodes[1].children.len == 1
}

fn test_argument_ordering() {
	doc := kdl.parse('v 1 2 3 a b c')!
	assert doc.nodes[0].entries.len == 6
}

fn test_spec_example_nested() {
	src := 'package {\n  name my-pkg\n  version "1.2.3"\n  dependencies {\n    lodash "^3.2.1" optional=#true alias=underscore\n  }\n}'
	doc := kdl.parse(src)!
	assert doc.nodes[0].name == 'package'
	assert doc.nodes[0].children.len == 3
}

fn test_spec_example_ci_config() {
	src := 'pipeline {\n  build {\n    image "rust:latest"\n    script "cargo build --release"\n  }\n  test {\n    image "rust:latest"\n    script "cargo test"\n  }\n}'
	doc := kdl.parse(src)!
	assert doc.nodes[0].children.len == 2
}

fn test_ident_plus_prefix() {
	doc := kdl.parse('+enabled')!
	assert doc.nodes[0].name == '+enabled'
}

fn test_ident_dot_prefix() {
	doc := kdl.parse('.hidden')!
	assert doc.nodes[0].name == '.hidden'
}

fn test_ident_plus_digit_is_number() {
	doc := kdl.parse('node +42')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 1
}

fn test_ident_minus_digit_is_number() {
	doc := kdl.parse('node -42')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 1
}

fn test_ident_only_dash() {
	doc := kdl.parse('-')!
	assert doc.nodes[0].name == '-'
}

fn test_nodes_separated_by_newlines() {
	doc := kdl.parse('my-node\n--flag\n.hidden')!
	assert doc.nodes.len == 3
}

fn test_underscore_leading_rejected() {
	doc := kdl.parse('v 0x_FF') or { kdl.Document{} }
	assert doc.nodes.len == 0
	doc2 := kdl.parse('v 0o_77') or { kdl.Document{} }
	assert doc2.nodes.len == 0
	doc3 := kdl.parse('v 0b_10') or { kdl.Document{} }
	assert doc3.nodes.len == 0
}

fn test_underscore_consecutive_accepted() {
	doc := kdl.parse('v 0xFF__FF')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 65535
}

fn test_underscore_trailing_accepted() {
	doc := kdl.parse('v 0xFF_')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 255
	doc2 := kdl.parse('v 0o77_')!
	assert kdl.as_int(doc2.nodes[0].entries[0].value) == 63
	doc3 := kdl.parse('v 0b10_')!
	assert kdl.as_int(doc3.nodes[0].entries[0].value) == 2
}

fn test_decimal_leading_zero_rejected() {
	doc := kdl.parse('v 07') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_fractional_without_integer_part_rejected() {
	doc := kdl.parse('v .5') or { kdl.Document{} }
	assert doc.nodes.len == 0
}

fn test_suffixed_decimal_rejected_in_strict() {
	doc := kdl.parse('timeout 10ms') or { kdl.Document{} }
	assert doc.nodes.len == 0
	doc2 := kdl.parse('dist 3.5km') or { kdl.Document{} }
	assert doc2.nodes.len == 0
}

fn test_inf_nan_value_types() {
	doc := kdl.parse('v #inf #-inf #nan')!
	e0 := doc.nodes[0].entries[0]
	assert kdl.as_f64(e0.value) > 1e300
	e1 := doc.nodes[0].entries[1]
	assert kdl.as_f64(e1.value) < -1e300
	e2 := doc.nodes[0].entries[2]
	assert kdl.as_f64(e2.value) != kdl.as_f64(e2.value) // NaN != NaN
}

fn test_negative_hex_parse() {
	doc := kdl.parse('v -0xFF')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == -255
}

fn test_negative_octal_parse() {
	doc := kdl.parse('v -0o10')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == -8
}

fn test_negative_binary_parse() {
	doc := kdl.parse('v -0b1010')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == -10
}

fn test_negative_hex_roundtrip() {
	doc := kdl.parse('v -0xFF')!
	out := kdl.format(doc)!
	assert out.contains('-0x')
	doc2 := kdl.parse(out)!
	assert kdl.as_int(doc2.nodes[0].entries[0].value) == -255
}

fn test_negative_octal_roundtrip() {
	doc := kdl.parse('v -0o10')!
	out := kdl.format(doc)!
	assert out.contains('-0o')
	doc2 := kdl.parse(out)!
	assert kdl.as_int(doc2.nodes[0].entries[0].value) == -8
}
