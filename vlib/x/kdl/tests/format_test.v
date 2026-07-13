module main

import x.kdl

struct TestPerson {
	name string
	age  int
}

struct TestArg {
	first string @[kdl: 'arg']
	last  string @[kdl: 'arg']
	age   int
}

struct TestArgs {
	items []string @[kdl: 'args']
}

struct TestOmitEmpty {
	name   string
	bio    string @[kdl: 'bio,omitempty']
	active bool
}

struct TestChildV {
	make  string @[kdl: 'arg']
	model string @[kdl: 'arg']
	color string
	year  int
}

struct TestGarage {
	car  TestChildV @[kdl: 'car,child']
	bike TestChildV @[kdl: 'bike,child']
}

fn test_format_roundtrip_basic() {
	src := 'my-node 1 2 key="val"'
	doc := kdl.parse(src)!
	out := kdl.format(doc)!
	assert out.len > 0
	doc2 := kdl.parse(out)!
	assert doc2.nodes.len == doc.nodes.len
}

fn test_format_roundtrip_complex() {
	src := 'package {\n  name my-pkg\n  version "1.2.3"\n  dependencies {\n    lodash "^3.2.1" optional=#true alias=underscore\n  }\n}'
	doc := kdl.parse(src)!
	out := kdl.format(doc)!
	doc2 := kdl.parse(out)!
	assert doc2.nodes.len == 1
	assert doc2.nodes[0].name == 'package'
}

fn test_format_empty_document() {
	doc := kdl.parse('')!
	out := kdl.format(doc)!
	assert out == ''
}

fn test_format_children() {
	doc := kdl.parse('parent {\n  child "val"\n}')!
	out := kdl.format(doc)!
	assert out.contains('{')
	assert out.contains('child')
}

fn test_encode_basic() {
	p := TestPerson{
		name: 'Alice'
		age:  30
	}
	out := kdl.encode(p)
	assert out.contains('TestPerson')
	assert out.contains('name=Alice')
	assert out.contains('age=30')
}

fn test_decode_basic() {
	p := kdl.decode[TestPerson]('TestPerson name=Alice age=30')!
	assert p.name == 'Alice'
	assert p.age == 30
}

fn test_encode_arg() {
	s := TestArg{
		first: 'Bob'
		last:  'Smith'
		age:   76
	}
	out := kdl.encode(s)
	assert out.contains('Bob')
	assert out.contains('Smith')
	assert out.contains('age=76')
}

fn test_decode_arg() {
	s := kdl.decode[TestArg]('TestArg Bob Smith age=76')!
	assert s.first == 'Bob'
	assert s.last == 'Smith'
	assert s.age == 76
}

fn test_encode_args() {
	it := TestArgs{
		items: ['a', 'b', 'c']
	}
	out := kdl.encode(it)
	assert out.contains('a')
	assert out.contains('b')
	assert out.contains('c')
}

fn test_decode_args() {
	it := kdl.decode[TestArgs]('TestArgs a b c')!
	assert it.items[0] == 'a'
	assert it.items[1] == 'b'
	assert it.items[2] == 'c'
}

fn test_encode_omitempty() {
	p := TestOmitEmpty{
		name:   'Jane'
		bio:    ''
		active: true
	}
	out := kdl.encode(p)
	assert !out.contains('bio')
}

fn test_encoder_reusable() {
	p := TestPerson{
		name: 'Test'
		age:  42
	}
	mut enc := kdl.new_encoder()
	out := enc.encode(p)
	assert out.contains('name=Test')
	assert out.contains('age=42')
}

fn test_rename_snake_case() {
	p := TestPerson{
		name: 'Alice'
		age:  30
	}
	opts := kdl.EncodeOpts{
		rename_all: 'snake_case'
	}
	out := kdl.encode_opts(p, opts)
	assert out.contains('test_person')
}

fn test_rename_kebab_case() {
	p := TestPerson{
		name: 'Alice'
		age:  30
	}
	opts := kdl.EncodeOpts{
		rename_all: 'kebab-case'
	}
	out := kdl.encode_opts(p, opts)
	assert out.contains('test-person')
}

fn test_encode_child() {
	g := TestGarage{
		car:  TestChildV{
			make:  'Ford'
			model: 'Mustang'
			color: 'red'
			year:  1967
		}
		bike: TestChildV{
			make:  'Honda'
			model: 'CB500'
			color: 'black'
			year:  2020
		}
	}
	out := kdl.encode(g)
	assert out.contains('car')
	assert out.contains('bike')
	assert out.contains('Mustang')
	assert out.contains('CB500')
}

fn test_document_manual_construction() {
	mut doc := kdl.Document{}
	mut config := kdl.Node{
		name: 'config'
	}
	config.entries << kdl.Property{
		key:   'host'
		value: kdl.StringVal{
			value: 'localhost'
			flag:  .quoted
		}
	}
	config.entries << kdl.Property{
		key:   'port'
		value: kdl.IntVal{
			value: 8080
		}
	}
	mut logging := kdl.Node{
		name: 'logging'
	}
	logging.entries << kdl.Property{
		key:   'level'
		value: kdl.StringVal{
			value: 'info'
			flag:  .bare
		}
	}
	config.children << logging
	doc.nodes << config
	out := kdl.format(doc)!
	assert out.contains('config')
	assert out.contains('host')
	assert out.contains('logging')
}

fn test_document_value_flag_enum() {
	_ := kdl.ValueFlag.none
	_ := kdl.ValueFlag.raw
	_ := kdl.ValueFlag.quoted
	_ := kdl.ValueFlag.bare
	_ := kdl.ValueFlag.binary
	_ := kdl.ValueFlag.octal
	_ := kdl.ValueFlag.hex
	_ := kdl.ValueFlag.scientific
	assert true
}

fn test_document_comment_type() {
	c := kdl.Comment{
		before: 'before'
		after:  'after'
	}
	assert c.before == 'before'
	assert c.after == 'after'
}

fn test_document_parse_error() {
	e := kdl.KdlParseError{
		line:   1
		column: 10
		offset: 0
		msg:    'error'
	}
	assert e.msg == 'error'
}

fn test_write_quoted_escapes_c0_controls() {
	mut doc := kdl.Document{}
	mut node := kdl.Node{
		name: 'x'
	}
	node.entries << kdl.Property{
		key:   'v'
		value: kdl.StringVal{
			value: '\x01'
			flag:  .quoted
		}
	}
	doc.nodes << node
	out := kdl.format(doc)!
	assert out.len > 4
	assert !out.contains('\x01')
}

fn test_write_quoted_escapes_del_once() {
	mut doc := kdl.Document{}
	mut node := kdl.Node{
		name: 'x'
	}
	node.entries << kdl.Property{
		key:   'v'
		value: kdl.StringVal{
			value: '\x7f'
			flag:  .quoted
		}
	}
	doc.nodes << node
	out := kdl.format(doc)!
	assert out.len > 4
	assert !out.contains('\x7f')
}

fn test_write_quoted_c1_control_roundtrip() {
	mut doc := kdl.Document{}
	mut node := kdl.Node{
		name: 'x'
	}
	node.entries << kdl.Property{
		key:   'v'
		value: kdl.StringVal{
			value: [u8(0xc2), 0x80].bytestr()
			flag:  .quoted
		}
	}
	doc.nodes << node
	out := kdl.format(doc)!
	doc2 := kdl.parse(out)!
	if val := kdl.property_get(&doc2.nodes[0], 'v') {
		s := kdl.as_string(val)
		assert s.len == 2
		assert s[0] == 0xc2
		assert s[1] == 0x80
	} else {
		assert false
	}
}

fn test_negative_hex_intval_roundtrip() {
	mut doc := kdl.Document{}
	mut node := kdl.Node{
		name: 'v'
	}
	node.entries << kdl.Argument{
		value: kdl.IntVal{
			value: -255
			flag:  .hex
		}
	}
	doc.nodes << node
	out := kdl.format(doc)!
	assert out.contains('-0x')
}

fn test_negative_octal_intval_roundtrip() {
	mut doc := kdl.Document{}
	mut node := kdl.Node{
		name: 'v'
	}
	node.entries << kdl.Argument{
		value: kdl.IntVal{
			value: -8
			flag:  .octal
		}
	}
	doc.nodes << node
	out := kdl.format(doc)!
	assert out.contains('-0o')
}

fn test_negative_binary_intval_roundtrip() {
	mut doc := kdl.Document{}
	mut node := kdl.Node{
		name: 'v'
	}
	node.entries << kdl.Argument{
		value: kdl.IntVal{
			value: -10
			flag:  .binary
		}
	}
	doc.nodes << node
	out := kdl.format(doc)!
	assert out.contains('-0b')
}
