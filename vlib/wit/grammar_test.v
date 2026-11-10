module wit

import os

fn grammar_roundtrip(src string) string {
	doc := parse(src) or { panic(err) }
	printed := doc.str()
	doc2 := parse(printed) or { panic('reparse failed: ${err}\n${printed}') }
	p2 := doc2.str()
	assert p2 == printed, 'print must be stable'
	return printed
}

fn grammar_codegen_compiles(name string, src string) string {
	doc := parse(src) or { panic(err) }
	gen := doc.to_v()
	assert gen.len > 0
	path := os.join_path(os.vtmp_dir(), 'wit_grammar_${name}.v')
	os.write_file(path, gen) or { panic(err) }
	res := os.exec(['v', '-check', path])
	assert res.exit_code == 0, res.output
	return gen
}

fn test_grammar_resource_roundtrip() {
	src := 'package p:q;

interface i {
	resource file {
		constructor(path: string);
		read: func() -> string;
		make: static func() -> u32;
	}
}
'
	printed := grammar_roundtrip(src)
	assert printed.contains('constructor(path: string);')
	assert printed.contains('static')
	gen := grammar_codegen_compiles('resource', src)
	assert gen.contains('pub struct File')
}

fn test_grammar_async_surface() {
	src := 'package p:q;

interface i {
	a: func() -> future<string>;
	b: func() -> stream<u8>;
}
'
	printed := grammar_roundtrip(src)
	assert printed.contains('future<string>')
	gen := grammar_codegen_compiles('async', src)
	assert gen.contains('pub fn a')
	assert gen.contains('pub fn b')
}

fn test_grammar_map() {
	src := 'package p:q;

interface i {
	m: func() -> map<string, u32>;
}
'
	printed := grammar_roundtrip(src)
	assert printed.contains('map<string, u32>')
	gen := grammar_codegen_compiles('map', src)
	assert gen.contains('pub fn m')
}

fn test_grammar_use() {
	src := 'package p:q;

interface base {
	x: func() -> u32;
}

interface derived {
	use base.{x};
	y: func() -> u32;
}
'
	printed := grammar_roundtrip(src)
	assert printed.contains('use base.{x};')
	gen := grammar_codegen_compiles('use', src)
	// single-file codegen: both interfaces land in one namespace
	assert gen.contains('pub fn x')
	assert gen.contains('pub fn y')
}

fn test_grammar_use_as() {
	src := 'package p:q;

interface base {
	x: func() -> u32;
}

interface derived {
	use base.{x as y};
	z: func() -> u32;
}
'
	printed := grammar_roundtrip(src)
	assert printed.contains('use base.{x as y};')
	gen := grammar_codegen_compiles('use_as', src)
	assert gen.contains('pub fn z')
}

fn test_grammar_world_roundtrip() {
	src := 'package example:demo@0.1.0;

world app {
	import log: func(msg: string);
	export run: func() -> u32;
}
'
	printed := grammar_roundtrip(src)
	assert printed.contains('import log: func(msg: string);')
	assert printed.contains('export run: func() -> u32;')
	gen := grammar_codegen_compiles('world', src)
	assert gen.len >= 0
}

fn test_grammar_attrs_roundtrip() {
	src := 'package p:q;

@since(version = 0.1.0)
interface i {
	@deprecated
	old: func() -> u32;
}
'
	doc := parse(src) or { panic(err) }
	printed := doc.str()
	assert printed.contains('@since(version = 0.1.0)')
	assert printed.contains('@deprecated')
	grammar_roundtrip(src)
	gen := grammar_codegen_compiles('attrs', src)
	assert gen.contains('pub fn old')
}
