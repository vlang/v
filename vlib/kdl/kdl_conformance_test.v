module kdl

// Conformance cases from the official KDL test suite, kdl-org/kdl `tests/test_cases`
// at commit 89c1087d5e7f530de328f18b6a0fad54ca8ea227 (2026-08-31),
// copyright Katerina Zoé Marchán Salvá and the KDL contributors,
// licensed under CC BY-SA 4.0 (https://creativecommons.org/licenses/by-sa/4.0/).
// Every `input` document is listed with the canonical re-serialisation the suite
// expects (`expected_kdl`), or with `must_fail: true` when the parser must reject it.
// The canonical form drops comments, sorts properties, writes strings bare when
// possible and numbers in decimal; see the suite README for the full rules.
// Generated file, do not edit by hand. To regenerate, run in a checkout of
// https://github.com/davlgd/vkdl at commit 5abb6deaf61a:
//     python3 tools/gen_conformance_test.py /path/to/v/vlib/kdl/kdl_conformance_test.v

struct ConformanceCase {
	name      string
	input     string
	expected  string
	must_fail bool
}

const conformance_cases = [
	ConformanceCase{
		name:     'all_escapes'
		input:    'node "\\"\\\\\\b\\f\\n\\r\\t\\s"\n'
		expected: 'node "\\"\\\\\\b\\f\\n\\r\\t "\n'
	},
	ConformanceCase{
		name:     'all_node_fields'
		input:    'node arg prop=val {\n    inner_node\n}\n'
		expected: 'node arg prop=val {\n    inner_node\n}\n'
	},
	ConformanceCase{
		name:     'arg_and_prop_same_name'
		input:    'node arg arg=val\n'
		expected: 'node arg arg=val\n'
	},
	ConformanceCase{
		name:     'arg_bare'
		input:    'node a\n'
		expected: 'node a\n'
	},
	ConformanceCase{
		name:     'arg_false_type'
		input:    'node (type)#false\n'
		expected: 'node (type)#false\n'
	},
	ConformanceCase{
		name:     'arg_float_type'
		input:    'node (type)2.5'
		expected: 'node (type)2.5\n'
	},
	ConformanceCase{
		name:     'arg_hex_type'
		input:    'node (type)0x10\n'
		expected: 'node (type)16\n'
	},
	ConformanceCase{
		name:     'arg_null_type'
		input:    'node (type)#null\n'
		expected: 'node (type)#null\n'
	},
	ConformanceCase{
		name:     'arg_raw_string_type'
		input:    'node (type)#"str"#\n'
		expected: 'node (type)str\n'
	},
	ConformanceCase{
		name:     'arg_string_type'
		input:    'node (type)"str"\n'
		expected: 'node (type)str\n'
	},
	ConformanceCase{
		name:     'arg_true_type'
		input:    'node (type)#true\n'
		expected: 'node (type)#true\n'
	},
	ConformanceCase{
		name:     'arg_type'
		input:    'node (type)arg\n'
		expected: 'node (type)arg\n'
	},
	ConformanceCase{
		name:     'arg_zero_type'
		input:    'node (type)0\n'
		expected: 'node (type)0\n'
	},
	ConformanceCase{
		name:     'asterisk_in_block_comment'
		input:    'node /* * */'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'bare_emoji'
		input:    '😁 happy!\n'
		expected: '😁 happy!\n'
	},
	ConformanceCase{
		name:     'bare_ident_dot'
		input:    'node .'
		expected: 'node .\n'
	},
	ConformanceCase{
		name:      'bare_ident_numeric_dot_fail'
		input:     'node .0n'
		must_fail: true
	},
	ConformanceCase{
		name:      'bare_ident_numeric_fail'
		input:     'node 0n'
		must_fail: true
	},
	ConformanceCase{
		name:      'bare_ident_numeric_sign_fail'
		input:     'node +0n'
		must_fail: true
	},
	ConformanceCase{
		name:     'bare_ident_sign'
		input:    'node +'
		expected: 'node +\n'
	},
	ConformanceCase{
		name:     'bare_ident_sign_dot'
		input:    'node +.'
		expected: 'node +.\n'
	},
	ConformanceCase{
		name:     'binary'
		input:    'node 0b10'
		expected: 'node 2\n'
	},
	ConformanceCase{
		name:     'binary_trailing_underscore'
		input:    'node 0b10_'
		expected: 'node 2\n'
	},
	ConformanceCase{
		name:     'binary_underscore'
		input:    'node 0b1_0\n'
		expected: 'node 2\n'
	},
	ConformanceCase{
		name:     'blank_arg_type'
		input:    'node ("")10'
		expected: 'node ("")10\n'
	},
	ConformanceCase{
		name:     'blank_node_type'
		input:    '("")node\n'
		expected: '("")node\n'
	},
	ConformanceCase{
		name:     'blank_prop_type'
		input:    'node key=("")#true\n'
		expected: 'node key=("")#true\n'
	},
	ConformanceCase{
		name:     'block_comment'
		input:    'node /* comment */ arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'block_comment_after_node'
		input:    'node /* hey */ arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'block_comment_before_node'
		input:    '/* hey */ node'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'block_comment_before_node_no_space'
		input:    '/* hey*/node\n'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'block_comment_newline'
		input:    '/* hey */\n'
		expected: '\n'
	},
	ConformanceCase{
		name:     'bom_initial'
		input:    '\ufeffnode arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:      'bom_later_fail'
		input:     'node \ufeffarg\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'boolean_arg'
		input:    'node #false #true\n'
		expected: 'node #false #true\n'
	},
	ConformanceCase{
		name:     'boolean_prop'
		input:    'node prop1=#true prop2=#false\n'
		expected: 'node prop1=#true prop2=#false\n'
	},
	ConformanceCase{
		name:     'braces_in_bare_id'
		input:    'foo123{bar}\n'
		expected: 'foo123 {\n    bar\n}\n'
	},
	ConformanceCase{
		name:     'chevrons_in_bare_id'
		input:    'foo123<bar>foo weeee\n'
		expected: 'foo123<bar>foo weeee\n'
	},
	ConformanceCase{
		name:     'comma_in_bare_id'
		input:    'foo123,bar weeee\n'
		expected: 'foo123,bar weeee\n'
	},
	ConformanceCase{
		name:     'comment_after_arg_type'
		input:    'node (type)/*hey*/10\n'
		expected: 'node (type)10\n'
	},
	ConformanceCase{
		name:     'comment_after_node_type'
		input:    '(type)/*hey*/node\n'
		expected: '(type)node\n'
	},
	ConformanceCase{
		name:     'comment_after_prop_type'
		input:    'node key=(type)/*hey*/10\n'
		expected: 'node key=(type)10\n'
	},
	ConformanceCase{
		name:     'comment_and_newline'
		input:    'node1 //\nnode2\n'
		expected: 'node1\nnode2\n'
	},
	ConformanceCase{
		name:     'comment_in_arg_type'
		input:    'node (type/*hey*/)10\n'
		expected: 'node (type)10\n'
	},
	ConformanceCase{
		name:     'comment_in_node_type'
		input:    '(type/*hey*/)node\n'
		expected: '(type)node\n'
	},
	ConformanceCase{
		name:     'comment_in_prop_type'
		input:    'node key=(type/*hey*/)10\n'
		expected: 'node key=(type)10\n'
	},
	ConformanceCase{
		name:     'commented_arg'
		input:    'node /- arg1 arg2\n'
		expected: 'node arg2\n'
	},
	ConformanceCase{
		name:     'commented_child'
		input:    'node arg /- {\n     inner_node\n}\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'commented_line'
		input:    '// node_1\nnode_2'
		expected: 'node_2\n'
	},
	ConformanceCase{
		name:     'commented_node'
		input:    '/- node_1\nnode_2\n/- node_3\n'
		expected: 'node_2\n'
	},
	ConformanceCase{
		name:     'commented_prop'
		input:    'node /- prop=val arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'crlf_between_nodes'
		input:    'node1\r\nnode2\r\n'
		expected: 'node1\nnode2\n'
	},
	ConformanceCase{
		name:     'dash_dash'
		input:    'node --\n'
		expected: 'node --\n'
	},
	ConformanceCase{
		name:      'dot_but_no_fraction_before_exponent_fail'
		input:     'node 1.e7'
		must_fail: true
	},
	ConformanceCase{
		name:      'dot_but_no_fraction_fail'
		input:     'node 1.'
		must_fail: true
	},
	ConformanceCase{
		name:      'dot_in_exponent_fail'
		input:     'node 1.0.0'
		must_fail: true
	},
	ConformanceCase{
		name:      'dot_zero_fail'
		input:     'node .0'
		must_fail: true
	},
	ConformanceCase{
		name:     'emoji'
		input:    'node 😀\n'
		expected: 'node 😀\n'
	},
	ConformanceCase{
		name:     'empty'
		input:    ''
		expected: '\n'
	},
	ConformanceCase{
		name:      'empty_arg_type_fail'
		input:     'node ()10\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'empty_child'
		input:    'node {\n}'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'empty_child_different_lines'
		input:    'node {\n}'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'empty_child_same_line'
		input:    'node {}'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'empty_child_whitespace'
		input:    'node {\n\n     }'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'empty_line_comment'
		input:    '//\nnode'
		expected: 'node\n'
	},
	ConformanceCase{
		name:      'empty_node_type_fail'
		input:     '()node\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'empty_prop_type_fail'
		input:     'node key=()#false\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'empty_quoted_node_id'
		input:    '"" arg\n'
		expected: '"" arg\n'
	},
	ConformanceCase{
		name:     'empty_quoted_prop_key'
		input:    'node ""=empty\n'
		expected: 'node ""=empty\n'
	},
	ConformanceCase{
		name:     'empty_string_arg'
		input:    'node ""\n'
		expected: 'node ""\n'
	},
	ConformanceCase{
		name:     'eof_after_escape'
		input:    'node \\'
		expected: 'node\n'
	},
	ConformanceCase{
		name:      'err_backslash_in_bare_id_fail'
		input:     'foo123\\bar weeee\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'esc_multiple_newlines'
		input:    'node "1\\\n\n\n2"\n'
		expected: 'node "12"\n'
	},
	ConformanceCase{
		name:     'esc_newline_in_string'
		input:    'node "hello\\nworld"'
		expected: 'node "hello\\nworld"\n'
	},
	ConformanceCase{
		name:     'esc_unicode_in_string'
		input:    'node "hello\\u{0a}world"\n'
		expected: 'node "hello\\nworld"\n'
	},
	ConformanceCase{
		name:     'escaped_whitespace'
		input:    '// All of these strings are the same\nnode \\\n\t"Hello\\n\\tWorld" \\\n\t"""\n\tHello\n\t\tWorld\n\t""" \\\n\t"Hello\\n\\      \\tWorld" \\\n\t"Hello\\n\\\n    \\tWorld" \\\n\t"Hello\\n\\t\\\n        World"\n\n// Note that this file deliberately mixes space and newline indentation for\n// test purposes\n'
		expected: 'node "Hello\\n\\tWorld" "Hello\\n\\tWorld" "Hello\\n\\tWorld" "Hello\\n\\tWorld" "Hello\\n\\tWorld"\n'
	},
	ConformanceCase{
		name:     'escline'
		input:    'node \\\n    arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'escline_after_semicolon'
		input:    'node; \\\nnode\n'
		expected: 'node\nnode\n'
	},
	ConformanceCase{
		name:     'escline_alone'
		input:    '\\\n'
		expected: '\n'
	},
	ConformanceCase{
		name:     'escline_empty_line'
		input:    '\\\n\nnode\n'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'escline_end_of_node'
		input:    'a \\\n\nb\n'
		expected: 'a\nb\n'
	},
	ConformanceCase{
		name:     'escline_in_child_block'
		input:    'parent {\n    child\n    \\ // comment\n    child\n}\n'
		expected: 'parent {\n    child\n    child\n}\n'
	},
	ConformanceCase{
		name:     'escline_line_comment'
		input:    'node \\   // comment\n    arg \\// comment\n    arg2\n'
		expected: 'node arg arg2\n'
	},
	ConformanceCase{
		name:     'escline_node'
		input:    'node1\n\\\nnode2\n'
		expected: 'node1\nnode2\n'
	},
	ConformanceCase{
		name:     'escline_node_type'
		input:    '\\\n(type)node\n'
		expected: '(type)node\n'
	},
	ConformanceCase{
		name:     'escline_slashdash'
		input:    'node\n\\\n/-\nnode\n'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'false_prefix_in_bare_id'
		input:    'false_id\n'
		expected: 'false_id\n'
	},
	ConformanceCase{
		name:     'false_prefix_in_prop_key'
		input:    'node false_id=1\n'
		expected: 'node false_id=1\n'
	},
	ConformanceCase{
		name:      'false_prop_key_fail'
		input:     'node false=1\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'floating_point_keyword_identifier_strings_fail'
		input:     'floats inf -inf nan\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'floating_point_keywords'
		input:    'floats #inf #-inf #nan\n'
		expected: 'floats #inf #-inf #nan\n'
	},
	ConformanceCase{
		name:      'hash_in_id_fail'
		input:     'foo#bar weee\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'hex'
		input:    'node 0xabcdef1234567890'
		expected: 'node 12379813812177893520\n'
	},
	ConformanceCase{
		name:     'hex_int'
		input:    'node 0xABCDEF0123456789abcdef\n'
		expected: 'node 207698809136909011942886895\n'
	},
	ConformanceCase{
		name:     'hex_int_underscores'
		input:    'node 0xABC_def_0123'
		expected: 'node 737894400291\n'
	},
	ConformanceCase{
		name:     'hex_leading_zero'
		input:    'node 0x01'
		expected: 'node 1\n'
	},
	ConformanceCase{
		name:      'illegal_char_in_binary_fail'
		input:     'node 0bx01\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'illegal_char_in_hex_fail'
		input:     'node 0x10g10'
		must_fail: true
	},
	ConformanceCase{
		name:      'illegal_char_in_octal_fail'
		input:     'node 0o45678'
		must_fail: true
	},
	ConformanceCase{
		name:     'initial_slashdash'
		input:    '/-node here\nanother-node\n'
		expected: 'another-node\n'
	},
	ConformanceCase{
		name:     'int_multiple_underscore'
		input:    'node 1_2_3_4'
		expected: 'node 1234\n'
	},
	ConformanceCase{
		name:     'just_block_comment'
		input:    '/* hey */'
		expected: '\n'
	},
	ConformanceCase{
		name:     'just_child'
		input:    'node {\n    inner_node     \n}'
		expected: 'node {\n    inner_node\n}\n'
	},
	ConformanceCase{
		name:     'just_newline'
		input:    '\n'
		expected: '\n'
	},
	ConformanceCase{
		name:     'just_node_id'
		input:    'node'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'just_space'
		input:    ' '
		expected: '\n'
	},
	ConformanceCase{
		name:      'just_space_in_arg_type_fail'
		input:     'node ( )#false\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'just_space_in_node_type_fail'
		input:     '( )node\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'just_space_in_prop_type_fail'
		input:     'node key=( )0x10\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'just_type_no_arg_fail'
		input:     'node (type)\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'just_type_no_node_id_fail'
		input:     '(type)\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'just_type_no_prop_fail'
		input:     'node key=(type)\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'leading_newline'
		input:    '\nnode'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'leading_zero_binary'
		input:    'node 0b01\n'
		expected: 'node 1\n'
	},
	ConformanceCase{
		name:     'leading_zero_int'
		input:    'node 011\n'
		expected: 'node 11\n'
	},
	ConformanceCase{
		name:     'leading_zero_oct'
		input:    'node 0o01\n'
		expected: 'node 1\n'
	},
	ConformanceCase{
		name:      'legacy_raw_string_fail'
		input:     'node r"foo"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'legacy_raw_string_hash_fail'
		input:     'node r#"foo"#\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'multiline_comment'
		input:    'node /*\nsome\ncomments\n*/ arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'multiline_nodes'
		input:    'node \\\n    arg1 \\// comment\n    arg2\n'
		expected: 'node arg1 arg2\n'
	},
	ConformanceCase{
		name:     'multiline_raw_string'
		input:    'node #"""\nhey\neveryone\nhow goes?\n"""#\n'
		expected: 'node "hey\\neveryone\\nhow goes?"\n'
	},
	ConformanceCase{
		name:     'multiline_raw_string_containing_quotes'
		input:    'node ##"""\n"""triple-quote"""\n##"too few quotes"##\n#"""too few #"""#\n"""##\n'
		expected: 'node "\\"\\"\\"triple-quote\\"\\"\\"\\n##\\"too few quotes\\"##\\n#\\"\\"\\"too few #\\"\\"\\"#"\n'
	},
	ConformanceCase{
		name:     'multiline_raw_string_empty'
		input:    'node #"""\n"""#'
		expected: 'node ""\n'
	},
	ConformanceCase{
		name:     'multiline_raw_string_empty_indented'
		input:    'node #"""\n\t"""#'
		expected: 'node ""\n'
	},
	ConformanceCase{
		name:     'multiline_raw_string_indented'
		input:    'node #"""\n    hey\n   everyone\n     how goes?\n  """#\n'
		expected: 'node "  hey\\n everyone\\n   how goes?"\n'
	},
	ConformanceCase{
		name:      'multiline_raw_string_non_matching_prefix_character_error_fail'
		input:     'node #"""\n    hey\n   everyone\n\t   how goes?\n  """#\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiline_raw_string_non_matching_prefix_count_error_fail'
		input:     'node #"""\n    hey\n everyone\n     how goes?\n  """#\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiline_raw_string_single_line_err_fail'
		input:     'node #"""one line"""#'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiline_raw_string_single_quote_err_fail'
		input:     'node #"\nhey\neveryone\nhow goes?\n"#\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'multiline_string'
		input:    'node """\nhey\neveryone\nhow goes?\n"""\n'
		expected: 'node "hey\\neveryone\\nhow goes?"\n'
	},
	ConformanceCase{
		name:     'multiline_string_containing_quotes'
		input:    'node """\nthis string contains "quotes", twice""\n"""\n'
		expected: 'node "this string contains \\"quotes\\", twice\\"\\""\n'
	},
	ConformanceCase{
		name:     'multiline_string_double_backslash'
		input:    'node """\na\\\\ b\na\\\\\\ b\n"""\n'
		expected: 'node "a\\\\ b\\na\\\\b"\n'
	},
	ConformanceCase{
		name:     'multiline_string_empty'
		input:    'node """\n"""'
		expected: 'node ""\n'
	},
	ConformanceCase{
		name:     'multiline_string_empty_indented'
		input:    'node """\n\t"""'
		expected: 'node ""\n'
	},
	ConformanceCase{
		name:     'multiline_string_escape_delimiter'
		input:    'node """\n\\"""\n"""\n'
		expected: 'node "\\"\\"\\""\n'
	},
	ConformanceCase{
		name:     'multiline_string_escape_in_closing_line'
		input:    'node """\n  foo \\\nbar\n  baz\n  \\   """\n'
		expected: 'node "foo bar\\nbaz"\n'
	},
	ConformanceCase{
		name:     'multiline_string_escape_in_closing_line_shallow'
		input:    'node """\n  foo \\\nbar\n  baz\n\\   """\n'
		expected: 'node "  foo bar\\n  baz"\n'
	},
	ConformanceCase{
		name:     'multiline_string_escape_newline_at_end'
		input:    'node """\n    a\n   \\\n"""\n'
		expected: 'node " a"\n'
	},
	ConformanceCase{
		name:      'multiline_string_escape_newline_at_end_fail'
		input:     'node """\na\n   \\\n"""\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiline_string_final_whitespace_escape_fail'
		input:     'node """\n  foo\n  bar\\\n  """'
		must_fail: true
	},
	ConformanceCase{
		name:     'multiline_string_indented'
		input:    'node """\n    hey\n   everyone\n     how goes?\n  """\n'
		expected: 'node "  hey\\n everyone\\n   how goes?"\n'
	},
	ConformanceCase{
		name:      'multiline_string_non_literal_prefix_fail'
		input:     'node """\n\\s escaped prefix\n  literal prefix\n  """\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiline_string_non_matching_prefix_character_error_fail'
		input:     'node """\n    hey\n   everyone\n\t   how goes?\n  """\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiline_string_non_matching_prefix_count_error_fail'
		input:     'node """\n    hey\n everyone\n     how goes?\n  """\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'multiline_string_only_1_quote'
		input:    'node """\n"\n"""\n'
		expected: 'node "\\""\n'
	},
	ConformanceCase{
		name:     'multiline_string_only_2_quotes'
		input:    'node """\n""\n"""\n'
		expected: 'node "\\"\\""\n'
	},
	ConformanceCase{
		name:      'multiline_string_single_line_err_fail'
		input:     'node """one line"""'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiline_string_single_quote_err_fail'
		input:     'node "\nhey\neveryone\nhow goes?\n"\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'multiline_string_whitespace_only'
		input:    '// This file deliberately contains unusual whitespace\n// The first two strings are empty\nnode """\n  \t""" """\n \t \\\n             \n \t """ """\n                            \n """\\\n    \\ // The next two strings contains only whitespace\n    """\n       \n       \n      \\s \n    """ #"""\n    \n\n  """#\n'
		expected: 'node "" "" "" "\\n\\n    " "\\n"\n'
	},
	ConformanceCase{
		name:     'multiline_string_wrapped_binary'
		input:    'node """\n    dead\\\n    beef\n    """\n'
		expected: 'node deadbeef\n'
	},
	ConformanceCase{
		name:      'multiple_dots_in_float_before_exponent_fail'
		input:     'node 1.0.0e7'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiple_dots_in_float_fail'
		input:     'node 1.0.0'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiple_es_in_float_fail'
		input:     'node 1.0E10e10\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'multiple_x_in_hex_fail'
		input:     'node 0xx10'
		must_fail: true
	},
	ConformanceCase{
		name:     'negative_exponent'
		input:    'node 1.0e-10'
		expected: 'node 1.0E-10\n'
	},
	ConformanceCase{
		name:     'negative_float'
		input:    'node -1.0 key=-10.0'
		expected: 'node -1.0 key=-10.0\n'
	},
	ConformanceCase{
		name:     'negative_int'
		input:    'node -10 prop=-15'
		expected: 'node -10 prop=-15\n'
	},
	ConformanceCase{
		name:     'nested_block_comment'
		input:    'node /* hi /* there */ everyone */ arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'nested_children'
		input:    'node1 {\n    node2 {\n        node\n    }\n}'
		expected: 'node1 {\n    node2 {\n        node\n    }\n}\n'
	},
	ConformanceCase{
		name:     'nested_comments'
		input:    'node /*/* nested */*/ arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'nested_multiline_block_comment'
		input:    "node /*\nhey /*\nhow's\n*/\n    it going\n    */ arg\n"
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'newline_between_nodes'
		input:    'node1\nnode2\n'
		expected: 'node1\nnode2\n'
	},
	ConformanceCase{
		name:     'newlines_in_block_comment'
		input:    'node /* hey so\nI was thinking\nabout newts */ arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'no_decimal_exponent'
		input:    'node 1e10'
		expected: 'node 1E+10\n'
	},
	ConformanceCase{
		name:      'no_digits_in_hex_fail'
		input:     'node 0x'
		must_fail: true
	},
	ConformanceCase{
		name:      'no_integer_digit_fail'
		input:     'node .1'
		must_fail: true
	},
	ConformanceCase{
		name:      'no_solidus_escape_fail'
		input:     'node "\\/"\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'node_false'
		input:    'node #false\n'
		expected: 'node #false\n'
	},
	ConformanceCase{
		name:     'node_true'
		input:    'node #true\n'
		expected: 'node #true\n'
	},
	ConformanceCase{
		name:     'node_type'
		input:    '(type)node'
		expected: '(type)node\n'
	},
	ConformanceCase{
		name:     'null_arg'
		input:    'node #null\n'
		expected: 'node #null\n'
	},
	ConformanceCase{
		name:     'null_prefix_in_bare_id'
		input:    'null_id\n'
		expected: 'null_id\n'
	},
	ConformanceCase{
		name:     'null_prefix_in_prop_key'
		input:    'node null_id=1\n'
		expected: 'node null_id=1\n'
	},
	ConformanceCase{
		name:     'null_prop'
		input:    'node prop=#null\n'
		expected: 'node prop=#null\n'
	},
	ConformanceCase{
		name:      'null_prop_key_fail'
		input:     'node null=1\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'numeric_arg'
		input:    'node 15.7'
		expected: 'node 15.7\n'
	},
	ConformanceCase{
		name:     'numeric_prop'
		input:    'node prop=10.0'
		expected: 'node prop=10.0\n'
	},
	ConformanceCase{
		name:     'octal'
		input:    'node 0o76543210'
		expected: 'node 16434824\n'
	},
	ConformanceCase{
		name:     'only_cr'
		input:    '\r'
		expected: '\n'
	},
	ConformanceCase{
		name:     'only_line_comment'
		input:    '// hi'
		expected: '\n'
	},
	ConformanceCase{
		name:     'only_line_comment_crlf'
		input:    '// comment\r\n'
		expected: '\n'
	},
	ConformanceCase{
		name:     'only_line_comment_newline'
		input:    '// hiiii\n'
		expected: '\n'
	},
	ConformanceCase{
		name:     'optional_child_semicolon'
		input:    'node {foo;bar;baz}\n'
		expected: 'node {\n    foo\n    bar\n    baz\n}\n'
	},
	ConformanceCase{
		name:      'parens_in_bare_id_fail'
		input:     'foo123(bar)foo weeee\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'parse_all_arg_types'
		input:    'node 1 1.0 1.0e10 1.0e-10 0x01 0o07 0b10 arg "arg" #"arg\\"# #true #false #null\n'
		expected: 'node 1 1.0 1.0E+10 1.0E-10 1 7 2 arg arg "arg\\\\" #true #false #null\n'
	},
	ConformanceCase{
		name:     'positive_exponent'
		input:    'node 1.0e+10'
		expected: 'node 1.0E+10\n'
	},
	ConformanceCase{
		name:     'positive_int'
		input:    'node +10'
		expected: 'node 10\n'
	},
	ConformanceCase{
		name:     'preserve_duplicate_nodes'
		input:    'node\nnode\n'
		expected: 'node\nnode\n'
	},
	ConformanceCase{
		name:     'preserve_node_order'
		input:    'node2\nnode5\nnode1'
		expected: 'node2\nnode5\nnode1\n'
	},
	ConformanceCase{
		name:     'prop_false_type'
		input:    'node key=(type)#false\n'
		expected: 'node key=(type)#false\n'
	},
	ConformanceCase{
		name:     'prop_float_type'
		input:    'node key=(type)2.5E10\n'
		expected: 'node key=(type)2.5E+10\n'
	},
	ConformanceCase{
		name:     'prop_hex_type'
		input:    'node key=(type)0x10\n'
		expected: 'node key=(type)16\n'
	},
	ConformanceCase{
		name:     'prop_identifier_type'
		input:    'node key=(type)str\n'
		expected: 'node key=(type)str\n'
	},
	ConformanceCase{
		name:     'prop_null_type'
		input:    'node key=(type)#null\n'
		expected: 'node key=(type)#null\n'
	},
	ConformanceCase{
		name:     'prop_raw_string_type'
		input:    'node key=(type)#"str"#\n'
		expected: 'node key=(type)str\n'
	},
	ConformanceCase{
		name:     'prop_string_type'
		input:    'node key=(type)"str"\n'
		expected: 'node key=(type)str\n'
	},
	ConformanceCase{
		name:     'prop_true_type'
		input:    'node key=(type)#true\n'
		expected: 'node key=(type)#true\n'
	},
	ConformanceCase{
		name:     'prop_type'
		input:    'node key=(type)#true\n'
		expected: 'node key=(type)#true\n'
	},
	ConformanceCase{
		name:     'prop_zero_type'
		input:    'node key=(type)0\n'
		expected: 'node key=(type)0\n'
	},
	ConformanceCase{
		name:     'question_mark_before_number'
		input:    'node ?15\n'
		expected: 'node ?15\n'
	},
	ConformanceCase{
		name:      'quote_in_bare_id_fail'
		input:     'foo123"bar weeee\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'quoted_arg_type'
		input:    'node ("type/")10'
		expected: 'node ("type/")10\n'
	},
	ConformanceCase{
		name:     'quoted_node_name'
		input:    '"0node"'
		expected: '"0node"\n'
	},
	ConformanceCase{
		name:     'quoted_node_type'
		input:    '("type/")node\n'
		expected: '("type/")node\n'
	},
	ConformanceCase{
		name:     'quoted_numeric'
		input:    'node prop="10.0"'
		expected: 'node prop="10.0"\n'
	},
	ConformanceCase{
		name:     'quoted_prop_name'
		input:    'node "0prop"=val\n'
		expected: 'node "0prop"=val\n'
	},
	ConformanceCase{
		name:     'quoted_prop_type'
		input:    'node key=("type/")#true\n'
		expected: 'node key=("type/")#true\n'
	},
	ConformanceCase{
		name:     'r_node'
		input:    'r "arg"\n'
		expected: 'r arg\n'
	},
	ConformanceCase{
		name:     'raw_arg_type'
		input:    'node (type)#true\n'
		expected: 'node (type)#true\n'
	},
	ConformanceCase{
		name:     'raw_node_name'
		input:    '#"\\node"#\n'
		expected: '"\\\\node"\n'
	},
	ConformanceCase{
		name:     'raw_node_type'
		input:    '(type)node'
		expected: '(type)node\n'
	},
	ConformanceCase{
		name:     'raw_prop_type'
		input:    'node key=(type)#true\n'
		expected: 'node key=(type)#true\n'
	},
	ConformanceCase{
		name:     'raw_string_arg'
		input:    'node_1 #""arg\\n"and #stuff"#\nnode_2 ##"#"arg\\n"#and #stuff"##\n'
		expected: 'node_1 "\\"arg\\\\n\\"and #stuff"\nnode_2 "#\\"arg\\\\n\\"#and #stuff"\n'
	},
	ConformanceCase{
		name:     'raw_string_backslash'
		input:    'node #"\\n"#\n'
		expected: 'node "\\\\n"\n'
	},
	ConformanceCase{
		name:     'raw_string_hash_no_esc'
		input:    'node #"#"#\n'
		expected: 'node "#"\n'
	},
	ConformanceCase{
		name:     'raw_string_just_backslash'
		input:    'node #"\\"#\n'
		expected: 'node "\\\\"\n'
	},
	ConformanceCase{
		name:      'raw_string_just_quote_fail'
		input:     '// This fails because `"""` MUST be followed by a newline.\nnode #"""#\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'raw_string_multiple_hash'
		input:    'node ###""#"##"###\n'
		expected: 'node "\\"#\\"##"\n'
	},
	ConformanceCase{
		name:     'raw_string_newline'
		input:    'node #"""\nhello\nworld\n"""#\n'
		expected: 'node "hello\\nworld"\n'
	},
	ConformanceCase{
		name:     'raw_string_prop'
		input:    'node_1 prop=#""arg#"\\n"#\nnode_2 prop=##"#"arg#"#\\n"##\n'
		expected: 'node_1 prop="\\"arg#\\"\\\\n"\nnode_2 prop="#\\"arg#\\"#\\\\n"\n'
	},
	ConformanceCase{
		name:     'raw_string_quote'
		input:    'node #"a"b"#\n'
		expected: 'node "a\\"b"\n'
	},
	ConformanceCase{
		name:     'repeated_arg'
		input:    'node arg arg\n'
		expected: 'node arg arg\n'
	},
	ConformanceCase{
		name:     'repeated_prop'
		input:    'node prop=10 prop=11'
		expected: 'node prop=11\n'
	},
	ConformanceCase{
		name:     'same_name_nodes'
		input:    'node\nnode\n'
		expected: 'node\nnode\n'
	},
	ConformanceCase{
		name:     'sci_notation_large'
		input:    'node prop=1.23E+1000'
		expected: 'node prop=1.23E+1000\n'
	},
	ConformanceCase{
		name:     'sci_notation_small'
		input:    'node prop=1.23E-1000'
		expected: 'node prop=1.23E-1000\n'
	},
	ConformanceCase{
		name:     'semicolon_after_child'
		input:    'node {\n     childnode\n};\n'
		expected: 'node {\n    childnode\n}\n'
	},
	ConformanceCase{
		name:     'semicolon_in_child'
		input:    'node1 {\n      node2;\n}'
		expected: 'node1 {\n    node2\n}\n'
	},
	ConformanceCase{
		name:      'semicolon_missing_after_children_fail'
		input:     'foo123{bar}foo weeee\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'semicolon_separated'
		input:    'node1;node2'
		expected: 'node1\nnode2\n'
	},
	ConformanceCase{
		name:     'semicolon_separated_nodes'
		input:    'node1; node2; '
		expected: 'node1\nnode2\n'
	},
	ConformanceCase{
		name:     'semicolon_terminated'
		input:    'node1;'
		expected: 'node1\n'
	},
	ConformanceCase{
		name:     'single_arg'
		input:    'node arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'single_prop'
		input:    'node prop=val\n'
		expected: 'node prop=val\n'
	},
	ConformanceCase{
		name:      'slash_in_bare_id_fail'
		input:     'foo123/bar weeee\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_after_arg_type_fail'
		input:     'node (ty)/-arg1 arg2\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_after_node_type_fail'
		input:     '(ty)/-node\nother-node\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_after_prop_key_fail'
		input:     'node key /- = value\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_after_prop_val_type_fail'
		input:     'node key=(ty)/-val other-arg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_after_type_fail'
		input:     'node (type) /- arg1 arg2\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'slashdash_arg_after_newline_esc'
		input:    'node \\\n    /- arg arg2\n'
		expected: 'node arg2\n'
	},
	ConformanceCase{
		name:     'slashdash_arg_before_newline_esc'
		input:    'node /-    \\\n    arg\n'
		expected: 'node\n'
	},
	ConformanceCase{
		name:      'slashdash_before_children_end_fail'
		input:     'node {\n    child1\n    /-\n}\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_before_eof_fail'
		input:     'node foo /-\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_before_prop_value_fail'
		input:     'node key = /-val etc\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_before_semicolon_fail'
		input:     'node foo /-;\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_between_child_blocks_fail'
		input:     'node { one } /- { two } { three }\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'slashdash_child'
		input:    'node /- {\n    node2\n}\n'
		expected: 'node\n'
	},
	ConformanceCase{
		name:      'slashdash_child_block_before_entry_err_fail'
		input:     'node /-{\n    child\n} foo {\n    bar\n}\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'slashdash_empty_child'
		input:    'node /- {\n}\n'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'slashdash_escline_before_arg_type'
		input:    'node /-\\\n(ty)arg1 arg2\n'
		expected: 'node arg2\n'
	},
	ConformanceCase{
		name:     'slashdash_escline_before_children'
		input:    'node arg1 /-\\\n{\n}\n'
		expected: 'node arg1\n'
	},
	ConformanceCase{
		name:     'slashdash_escline_before_node'
		input:    '/-\\\nnode1\nnode2\n'
		expected: 'node2\n'
	},
	ConformanceCase{
		name:     'slashdash_false_node'
		input:    'node foo /-\nnot-a-node bar\n'
		expected: 'node foo bar\n'
	},
	ConformanceCase{
		name:     'slashdash_full_node'
		input:    '/- node 1.0 "a" b="""\nb\n"""\n'
		expected: '\n'
	},
	ConformanceCase{
		name:     'slashdash_in_slashdash'
		input:    '/- node1 /- 1.0\nnode2'
		expected: 'node2\n'
	},
	ConformanceCase{
		name:      'slashdash_inside_arg_type_fail'
		input:     'node (/-bad)nope\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'slashdash_inside_node_type_fail'
		input:     '(/-ty)node\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'slashdash_multi_line_comment_entry'
		input:    'node 1 /- /*\nmulti\nline\ncomment\nhere\n*/ 2 3\n'
		expected: 'node 1 3\n'
	},
	ConformanceCase{
		name:     'slashdash_multi_line_comment_inline'
		input:    'node 1 /-/*two*/2 3\n'
		expected: 'node 1 3\n'
	},
	ConformanceCase{
		name:     'slashdash_multiple_child_blocks'
		input:    'node foo /-{\n    one\n} \\\n/-{\n    two\n} {\n    three\n} /-{\n    four\n}\n'
		expected: 'node foo {\n    three\n}\n'
	},
	ConformanceCase{
		name:     'slashdash_negative_number'
		input:    'node /--1.0 2.0'
		expected: 'node 2.0\n'
	},
	ConformanceCase{
		name:     'slashdash_newline_before_children'
		input:    'node 1 2 /-\n{\n    child\n}\n'
		expected: 'node 1 2\n'
	},
	ConformanceCase{
		name:     'slashdash_newline_before_entry'
		input:    'node 1 /-\n2 3\n'
		expected: 'node 1 3\n'
	},
	ConformanceCase{
		name:     'slashdash_newline_before_node'
		input:    '/-\nnode 1 2 3\n'
		expected: '\n'
	},
	ConformanceCase{
		name:     'slashdash_node_in_child'
		input:    'node1 {\n    /- node2\n}'
		expected: 'node1\n'
	},
	ConformanceCase{
		name:     'slashdash_node_with_child'
		input:    '/- node {\n   node2\n}'
		expected: '\n'
	},
	ConformanceCase{
		name:     'slashdash_only_node'
		input:    '/-node\n'
		expected: '\n'
	},
	ConformanceCase{
		name:     'slashdash_only_node_with_space'
		input:    '/- node'
		expected: '\n'
	},
	ConformanceCase{
		name:     'slashdash_prop'
		input:    'node /- key=value arg\n'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'slashdash_raw_prop_key'
		input:    'node /- key=value\n'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'slashdash_repeated_prop'
		input:    'node arg=correct /- arg=wrong\n'
		expected: 'node arg=correct\n'
	},
	ConformanceCase{
		name:     'slashdash_single_line_comment_entry'
		input:    'node 1 /- // stuff\n2 3\n'
		expected: 'node 1 3\n'
	},
	ConformanceCase{
		name:     'slashdash_single_line_comment_node'
		input:    '/- // this is a comment\nnode1\nnode2\n'
		expected: 'node2\n'
	},
	ConformanceCase{
		name:     'space_after_arg_type'
		input:    'node (type) 10\n'
		expected: 'node (type)10\n'
	},
	ConformanceCase{
		name:     'space_after_node_type'
		input:    '(type) node\n'
		expected: '(type)node\n'
	},
	ConformanceCase{
		name:     'space_after_prop_type'
		input:    'node key=(type) #false\n'
		expected: 'node key=(type)#false\n'
	},
	ConformanceCase{
		name:     'space_around_prop_marker'
		input:    'node foo = bar\n'
		expected: 'node foo=bar\n'
	},
	ConformanceCase{
		name:     'space_in_arg_type'
		input:    'node (type )#false\n'
		expected: 'node (type)#false\n'
	},
	ConformanceCase{
		name:     'space_in_node_type'
		input:    '( type)node\n'
		expected: '(type)node\n'
	},
	ConformanceCase{
		name:     'space_in_prop_type'
		input:    'node key=(type )#false\n'
		expected: 'node key=(type)#false\n'
	},
	ConformanceCase{
		name:      'square_bracket_in_bare_id_fail'
		input:     'foo123[bar]foo weeee\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'string_arg'
		input:    'node "arg"'
		expected: 'node arg\n'
	},
	ConformanceCase{
		name:     'string_escaped_literal_whitespace'
		input:    'node "Hello \\\nWorld \\          Stuff"\n'
		expected: 'node "Hello World Stuff"\n'
	},
	ConformanceCase{
		name:     'string_prop'
		input:    'node prop="val"'
		expected: 'node prop=val\n'
	},
	ConformanceCase{
		name:     'tab_space'
		input:    'node\n'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'trailing_crlf'
		input:    'node\r\n'
		expected: 'node\n'
	},
	ConformanceCase{
		name:     'trailing_underscore_hex'
		input:    'node 0x123abc_'
		expected: 'node 1194684\n'
	},
	ConformanceCase{
		name:     'trailing_underscore_octal'
		input:    'node 0o123_\n'
		expected: 'node 83\n'
	},
	ConformanceCase{
		name:     'true_prefix_in_bare_id'
		input:    'true_id\n'
		expected: 'true_id\n'
	},
	ConformanceCase{
		name:     'true_prefix_in_prop_key'
		input:    'node true_id=1\n'
		expected: 'node true_id=1\n'
	},
	ConformanceCase{
		name:      'true_prop_key_fail'
		input:     'node true=1\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'two_nodes'
		input:    'node1\nnode2\n'
		expected: 'node1\nnode2\n'
	},
	ConformanceCase{
		name:      'type_before_prop_key_fail'
		input:     'node (type)key=10\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unbalanced_raw_hashes_fail'
		input:     'node ##"foo"#\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'underscore_at_start_of_fraction_fail'
		input:     'node 1._7'
		must_fail: true
	},
	ConformanceCase{
		name:      'underscore_at_start_of_hex_fail'
		input:     'node 0x_10'
		must_fail: true
	},
	ConformanceCase{
		name:     'underscore_before_number'
		input:    'node _15\n'
		expected: 'node _15\n'
	},
	ConformanceCase{
		name:     'underscore_in_exponent'
		input:    'node 1.0e-10_0\n'
		expected: 'node 1.0E-100\n'
	},
	ConformanceCase{
		name:     'underscore_in_float'
		input:    'node 1_1.0\n'
		expected: 'node 11.0\n'
	},
	ConformanceCase{
		name:     'underscore_in_fraction'
		input:    'node 1.0_2'
		expected: 'node 1.02\n'
	},
	ConformanceCase{
		name:     'underscore_in_int'
		input:    'node 1_0\n'
		expected: 'node 10\n'
	},
	ConformanceCase{
		name:     'underscore_in_octal'
		input:    'node 0o012_3456_7'
		expected: 'node 342391\n'
	},
	ConformanceCase{
		name:      'unicode_delete_fail'
		input:     '// 0x007F (Delete)\nnode1 arg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_escaped_above_max_fail'
		input:     'no "Higher than max Unicode Scalar Value \\u{10FFFF} \\u{11FFFF}"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_escaped_h1_fail'
		input:     'no "Surrogates high\\u{D800}"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_escaped_h2_fail'
		input:     'no "Surrogates high\\u{D911}"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_escaped_h3_fail'
		input:     'no "Surrogates high\\u{DABB}"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_escaped_h4_fail'
		input:     'no "Surrogates high\\u{DBFF}"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_escaped_l1_fail'
		input:     'no "Surrogates low\\u{DC00}"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_escaped_l2_fail'
		input:     'no "Surrogates low\\u{DEAD}"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_escaped_l3_fail'
		input:     'eno "Surrogates low\\u{DFFF}"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_escaped_too_long_lead0_fail'
		input:     'no "Even with leading 0s Unicode Scalar Value escapes must ≤6: \\u{0012345}"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_fsi_fail'
		input:     '// 0x2068\nnode1 \u2068arg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_lre_fail'
		input:     '// 0x202A\nnode1 \u202aarg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_lri_fail'
		input:     '// 0x2066\nnode1\u2066arg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_lrm_fail'
		input:     '// 0x200E\nnode \u200earg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_lro_fail'
		input:     '// 0x202D\nnode \u202darg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_pdf_fail'
		input:     '// 0x202C\nnode \u202carg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_pdi_fail'
		input:     '// 0x2069\nnode \u2069arg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_rle_fail'
		input:     '// 0x202B\nnode1 \u202barg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_rli_fail'
		input:     '// 0x2067\nnode1 \u2067arg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_rlm_fail'
		input:     '// 0x200F\nnode \u200farg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unicode_rlo_fail'
		input:     '// 0x202E\nnode \u202earg\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'unicode_silly'
		input:    'ノード　お名前=ฅ^•ﻌ•^ฅ\n'
		expected: 'ノード お名前=ฅ^•ﻌ•^ฅ\n'
	},
	ConformanceCase{
		name:      'unicode_under_0x20_fail'
		input:     '// 0x0019\nnode1 \x19arg\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'unterminated_empty_node_fail'
		input:     'node {\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'unusual_bare_id_chars_in_quoted_id'
		input:    '"foo123~!@\$%^&*.:\'|?+<>,`-_" weeee\n'
		expected: "foo123~!@\$%^&*.:'|?+<>,`-_ weeee\n"
	},
	ConformanceCase{
		name:     'unusual_chars_in_bare_id'
		input:    "foo123~!@\$%^&*.:'|?+<>,`-_ weeee\n"
		expected: "foo123~!@\$%^&*.:'|?+<>,`-_ weeee\n"
	},
	ConformanceCase{
		name:     'vertical_tab_whitespace'
		input:    'node arg\x0bnode2 arg2\n'
		expected: 'node arg\nnode2 arg2\n'
	},
	ConformanceCase{
		name:     'zero_float'
		input:    'node 0.0\n'
		expected: 'node 0.0\n'
	},
	ConformanceCase{
		name:     'zero_int'
		input:    'node 0\n'
		expected: 'node 0\n'
	},
	ConformanceCase{
		name:      'zero_space_before_first_arg_fail'
		input:     'node"string"\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'zero_space_before_prop_fail'
		input:     'node foo="value"bar=5\n'
		must_fail: true
	},
	ConformanceCase{
		name:      'zero_space_before_second_arg_fail'
		input:     'node "string"1\n'
		must_fail: true
	},
	ConformanceCase{
		name:     'zero_space_before_slashdash_arg'
		input:    'node "string"/-1\n'
		expected: 'node string\n'
	},
	ConformanceCase{
		name:     'zero_space_before_slashdash_children'
		input:    'node "string"/-{}\nnode "string" {}/-{}\n'
		expected: 'node string\nnode string\n'
	},
	ConformanceCase{
		name:     'zero_space_before_slashdash_prop'
		input:    'node "string"/-foo=1\n'
		expected: 'node string\n'
	},
]!

// Every valid document must serialise to the expected canonical form and
// every `must_fail` document must be rejected.
fn test_official_suite() {
	assert conformance_cases.len == 338
	mut failures := []string{}
	for c in conformance_cases {
		name := c.name
		doc := parse(c.input) or {
			if !c.must_fail {
				failures << '${name}: unexpected parse error: ${err.msg()}'
			}
			continue
		}
		if c.must_fail {
			failures << '${name}: should have been rejected'
			continue
		}
		// the suite writes an empty document as a single newline; the writer emits nothing
		expected := if c.expected == '\n' { '' } else { c.expected }
		actual := doc.str()
		if actual != expected && !same_modulo_floats(actual, expected) {
			failures << '${name}:\n    expected: ${expected.replace('\n', '\\n')}\n    actual:   ${actual.replace('\n', '\\n')}'
		}
	}
	for f in failures {
		eprintln(f)
	}
	assert failures.len == 0, '${failures.len} failures in the official suite'
}

// Every valid document of the suite must survive parse -> str -> parse.
fn test_roundtrip_official_suite() {
	mut checked := 0
	for c in conformance_cases {
		if c.must_fail {
			continue
		}
		name := c.name
		doc := parse(c.input) or { panic('${name}: ${err.msg()}') }
		text := doc.str()
		again := parse(text) or { panic('${name}: re-parse failed: ${err.msg()}\n${text}') }
		assert again.equals(doc), 'round trip changed ${name}'
		assert again.str() == text, 'second serialisation differs for ${name}'
		checked++
	}
	assert checked == 243
}

// same_modulo_floats compares two canonical documents token by token, allowing
// float tokens that differ only in notation (1.0E+10 vs 1E+10) or that overflow f64.
// Quoted strings are single tokens and must match exactly.
fn same_modulo_floats(a string, b string) bool {
	ta := canonical_tokens(a)
	tb := canonical_tokens(b)
	if ta.len != tb.len {
		return false
	}
	for i in 0 .. ta.len {
		if ta[i] == tb[i] {
			continue
		}
		pa, na := split_number_tail(ta[i])
		pb, nb := split_number_tail(tb[i])
		if pa != pb || !is_float_token(na) || !is_float_token(nb) {
			return false
		}
		fa := float_token(na)
		fb := float_token(nb)
		if fa != fb && !(fa == 0 && fb == 0) {
			return false
		}
	}
	return true
}

// canonical_tokens splits canonical KDL text on whitespace, keeping quoted
// strings (with their escapes) intact.
fn canonical_tokens(s string) []string {
	mut toks := []string{}
	mut i := 0
	for i < s.len {
		if s[i] == `\n` {
			// newlines terminate nodes: keep them so structure is compared too
			toks << '\n'
			i++
			continue
		}
		if s[i] == ` ` {
			i++
			continue
		}
		start := i
		mut in_quotes := false
		for i < s.len {
			c := s[i]
			if in_quotes {
				if c == `\\` {
					i += 2
					continue
				}
				if c == `"` {
					in_quotes = false
				}
			} else if c == `"` {
				in_quotes = true
			} else if c == ` ` || c == `\n` {
				break
			}
			i++
		}
		toks << s[start..i]
	}
	return toks
}

fn test_canonical_tokens_respect_quotes() {
	assert canonical_tokens('n "a  b" k="x y" 1.0\n') == ['n', '"a  b"', 'k="x y"', '1.0', '\n']
	// structure: node boundaries and children blocks must match
	assert !same_modulo_floats('n\nm\n', 'n m\n')
	assert !same_modulo_floats('n {\n    m\n}\n', 'n m\n')
	assert !same_modulo_floats('n {\n    m\n}\n', 'n\nm\n')
	assert !same_modulo_floats('n {\n    m\n    o\n}\n', 'n {\n    m {\n        o\n    }\n}\n')
	assert same_modulo_floats('n 1.0 {\n    m 2.0\n}\n', 'n 1.00 {\n    m 2.00\n}\n')
	assert !same_modulo_floats('n "a  b"\n', 'n "a b"\n')
	assert !same_modulo_floats('n "a 1.0000000000000001 b"\n', 'n "a 1.0 b"\n')
	assert !same_modulo_floats('n 1.5\n', 'n 1.6\n')
	assert same_modulo_floats('n 1.0E+10 k=(t)2.5E+10\n', 'n 1E+10 k=(t)2.5e10\n')
	// numbers inside strings are strings: no tolerance
	assert !same_modulo_floats('n "a=1.0000000000000001"\n', 'n "a=1.0"\n')
	assert !same_modulo_floats('n "(t)1.0000000000000001"\n', 'n "(t)1.0"\n')
	assert !same_modulo_floats('n k="1.0000000000000001"\n', 'n k="1.0"\n')
	assert !same_modulo_floats('n "1.0000000000000001"\n', 'n "1.0"\n')
	assert !same_modulo_floats('n 1.0000000000000001x\n', 'n 1.0x\n')
	assert !same_modulo_floats('n 10\n', 'n 10.0\n')
	assert same_modulo_floats('n "a=b"=1.0\n', 'n "a=b"=1.00\n')
}

fn split_number_tail(tok string) (string, string) {
	mut i := tok.len - 1
	for i >= 0 && tok[i] !in [u8(`=`), `)`] {
		i--
	}
	return tok[..i + 1], tok[i + 1..]
}

// Floats outside the f64 range are stored as infinity, so `#inf` may stand for
// a huge literal such as 1.23E+1000.
fn is_float_token(s string) bool {
	if s == '#inf' || s == '#-inf' {
		return true
	}
	if s.len == 0 || !looks_like_number(s) {
		return false
	}
	// the whole token must be a float according to the number grammar, so a
	// quote or any other trailing character disqualifies it
	d := parse_number(s) or { return false }
	return d is f64
}

fn float_token(s string) f64 {
	return match s {
		'#inf' { f64_inf }
		'#-inf' { -f64_inf }
		else { s.f64() }
	}
}
