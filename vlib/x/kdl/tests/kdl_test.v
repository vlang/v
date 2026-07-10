module main

import x.kdl
import os

fn test_basic_parse() {
	doc := kdl.parse('name "Alice" age 30 active #true extra #null')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'name'
}

fn test_parse_with_children() {
	doc := kdl.parse('parent {\n  child1 "a"\n  child2 "b"\n}')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].children.len == 2
	assert doc.nodes[0].children[0].name == 'child1'
}

fn test_parse_properties() {
	doc := kdl.parse('node key1="val1" key2=42 key3=#true')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 3
}

fn test_parse_numbers() {
	doc := kdl.parse('nums 0 42 -17 0xFF 0o77 0b1010 3.14 1e10')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 8
}

fn test_parse_bool_and_null() {
	doc := kdl.parse('flags #true #false #null')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 3
}

fn test_parse_empty() {
	doc := kdl.parse('')!
	assert doc.nodes.len == 0
}

fn test_parse_comments() {
	doc := kdl.parse('// comment\nnode "value"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
}

fn test_parse_block_comment() {
	doc := kdl.parse('/* block */node "x"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
}

fn test_parse_slashdash() {
	doc := kdl.parse('/- commented "node"\nreal "value"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'real'
}

fn test_parse_semicolons() {
	doc := kdl.parse('a "1"; b "2"')!
	assert doc.nodes.len == 2
	assert doc.nodes[0].name == 'a'
	assert doc.nodes[1].name == 'b'
}

fn test_parse_file() {
	tmp := os.join_path(os.temp_dir(), 'kdl_parse_file.kdl')
	os.write_file(tmp, 'config {\n  port 8080\n}')!
	doc := kdl.parse_file(tmp)!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'config'
	os.rm(tmp)!
}

fn test_format_roundtrip() {
	src := 'my-node 1 2 key="val"'
	doc := kdl.parse(src)!
	out := kdl.format(doc)!
	assert out.len > 0
	doc2 := kdl.parse(out)!
	assert doc2.nodes.len == doc.nodes.len
}

fn test_format_empty() {
	doc := kdl.parse('')!
	out := kdl.format(doc)!
	_ := out
}

fn test_value_types() {
	doc := kdl.parse('vals "str" 42 3.14 #true #false #null')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 6
}

fn test_empty_children() {
	doc := kdl.parse('node {}')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].children.len == 0
}

fn test_node_names() {
	doc := kdl.parse('my-node --flag .hidden')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 2
}

fn test_nodes_separated_by_newlines() {
	doc := kdl.parse('my-node\n--flag\n.hidden')!
	assert doc.nodes.len == 3
	assert doc.nodes[0].name == 'my-node'
	assert doc.nodes[1].name == '--flag'
	assert doc.nodes[2].name == '.hidden'
}

fn test_kdl_spec_example() {
	src := 'package {
  name my-pkg
  version "1.2.3"
  dependencies {
    lodash "^3.2.1" optional=#true alias=underscore
  }
}'
	doc := kdl.parse(src)!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'package'
}

struct StaffM {
	first string @[kdl: 'arg']
	last  string @[kdl: 'arg']
	age   int
}

fn test_marshal_arg() {
	s := StaffM{
		first: 'Bob'
		last:  'Smith'
		age:   76
	}
	out := kdl.encode(s)
	assert out.contains('Bob')
	assert out.contains('Smith')
	assert out.contains('age')
}

struct ItemsM {
	items []string @[kdl: 'args']
}

fn test_marshal_args() {
	it := ItemsM{
		items: ['a', 'b', 'c']
	}
	out := kdl.encode(it)
	assert out.contains('a')
	assert out.contains('b')
	assert out.contains('c')
}

struct PersonM {
	name string
	age  int
}

fn test_unmarshal_basic() {
	p := kdl.decode[PersonM]('PersonM name=Alice age=30')!
	assert p.name == 'Alice'
	assert p.age == 30
}

struct StaffDec {
	first string @[kdl: 'arg']
	last  string @[kdl: 'arg']
	age   int
}

fn test_unmarshal_arg() {
	s := kdl.decode[StaffDec]('StaffDec Bob Smith age=76')!
	assert s.first == 'Bob'
	assert s.last == 'Smith'
	assert s.age == 76
}

struct ItemsDec {
	items []string @[kdl: 'args']
}

fn test_unmarshal_args() {
	it := kdl.decode[ItemsDec]('ItemsDec a b c')!
	assert it.items[0] == 'a'
	assert it.items[1] == 'b'
	assert it.items[2] == 'c'
}

struct ProfileM {
	name   string
	bio    string @[kdl: 'bio,omitempty']
	active bool
}

fn test_marshal_omitempty() {
	p := ProfileM{
		name:   'Jane'
		bio:    ''
		active: true
	}
	out := kdl.encode(p)
	assert !out.contains('bio')
}

fn test_encoder_new() {
	p := PersonM{
		name: 'Test'
		age:  42
	}
	mut enc := kdl.new_encoder()
	out := enc.encode(p)
	assert out.contains('name=Test')
	assert out.contains('age=42')
}

fn test_parse_suffixed_decimal() {
	doc := kdl.parse('timeout 10ms')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'timeout'
	assert doc.nodes[0].entries.len == 1
}

fn test_parse_suffixed_kib() {
	doc := kdl.parse('memory 5KiB')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 1
}

fn test_parse_underscore_numbers() {
	doc := kdl.parse('nums 1_000 0xFF_FF 1_000_000')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 3
}

fn test_parse_line_continuation() {
	doc := kdl.parse('node \\\n  arg1 arg2')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
	assert doc.nodes[0].entries.len == 2
}

fn test_coerce_as_string() {
	doc := kdl.parse('v "hello"')!
	e := doc.nodes[0].entries[0]
	assert kdl.as_string(e.value) == 'hello'
}

fn test_coerce_as_int() {
	doc := kdl.parse('v 42')!
	e := doc.nodes[0].entries[0]
	assert kdl.as_int(e.value) == 42
}

fn test_coerce_as_bool() {
	doc := kdl.parse('v #true')!
	e := doc.nodes[0].entries[0]
	assert kdl.as_bool(e.value) == true
}

fn test_coerce_is_null() {
	doc := kdl.parse('v #null')!
	e := doc.nodes[0].entries[0]
	assert kdl.is_null(e.value) == true
}

fn test_unicode_nbsp_whitespace() {
	doc := kdl.parse('node\xc2\xa0"val"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_ideographic_whitespace() {
	doc := kdl.parse('node\xe3\x80\x80"val"')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].name == 'node'
}

fn test_relaxed_flags_available() {
	_ := kdl.nginx_syntax
	_ := kdl.yaml_toml_assignments
	assert true
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

fn test_hashed_keywords_valid() {
	doc := kdl.parse('v #true #false #null')!
	assert doc.nodes.len == 1
	assert doc.nodes[0].entries.len == 3
}

fn test_keyword_number_inf() {
	doc := kdl.parse('v #inf')!
	e := doc.nodes[0].entries[0]
	assert kdl.as_string(e.value).len > 0
}

fn test_keyword_number_ninf() {
	doc := kdl.parse('v #-inf')!
	e := doc.nodes[0].entries[0]
	assert kdl.as_string(e.value).len > 0
}

fn test_keyword_number_nan() {
	doc := kdl.parse('v #nan')!
	e := doc.nodes[0].entries[0]
	assert kdl.as_string(e.value).len > 0
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

fn test_raw_string_multihash() {
	doc := kdl.parse('md #"literal\\n"#')!
	e := doc.nodes[0].entries[0]
	assert kdl.as_string(e.value).len > 0
}

fn test_raw_string_empty() {
	doc := kdl.parse('md #""#')!
	e := doc.nodes[0].entries[0]
	assert kdl.as_string(e.value) == ''
}
