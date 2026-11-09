module wit

import os

fn test_parse_basic() {
	src := '
package example:demo@0.1.0;

interface types {
	record point {
		x: f64,
		y: f64,
	}
	enum color { red, green, blue }
	variant shape { circle(f64), empty }
	flags perms { read, write }
	type alias = u32;

	add: func(a: s32, b: s32) -> s32;
	greet: func(name: string) -> string;
}

world app {
	import types;
	export run: func();
}
'
	doc := parse(src) or { panic(err) }
	assert doc.package.namespace == 'example'
	assert doc.package.name == 'demo'
	assert doc.interfaces.len == 1
	iface := doc.interfaces[0]
	assert iface.name == 'types'
	assert iface.items.len == 7
	assert doc.worlds.len == 1
	w := doc.worlds[0]
	assert w.name == 'app'
	assert w.imports.len == 1
	assert w.exports.len == 1
	// round-trip type str
	assert prim_type('s32').str() == 's32'

	generated := doc.to_v()
	assert generated.contains('pub struct Point')
	assert generated.contains('pub fn add')

	doc2 := parse(src) or { panic(err) }
	printed := doc2.str()
	assert printed.contains('interface types')
	assert printed.contains('record point')
}

fn test_wasi_fs_types_codegen_compiles() {
	src := os.read_file('vlib/wasm/wit/testdata/wasi/fs_types.wit') or { panic(err) }
	doc := parse(src) or { panic(err) }
	generated := doc.to_v()
	assert generated.len > 0
	os.write_file('vlib/wasm/wit/testdata/wasi/fs_types_generated.v', generated) or { panic(err) }
	res := os.exec(['v', '-check', 'vlib/wasm/wit/testdata/wasi/fs_types_generated.v'])
	assert res.exit_code == 0, res.output
}

fn test_validate_unknown_type() {
	src := 'package p:q;

interface i {
	f: func(x: unknown_type) -> u32;
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or {
		assert err.msg().contains('error_unknown_type')
		return
	}
	panic('expected error')
}

fn test_validate_duplicate_name() {
	src := 'package p:q;

interface i {
	x: func() -> u32;
	x: func() -> u32;
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or {
		assert err.msg().contains('error_duplicate_name')
		return
	}
	panic('expected error')
}

fn test_validate_valid() {
	src := 'package p:q;

interface i {
	f: func(x: u32) -> u32;
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or { panic(err) }
}

fn test_validate_duplicate_method() {
	src := 'package p:q;

interface i {
	resource r {
		m: func() -> u32;
		m: func() -> u32;
	}
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or {
		assert err.msg().contains('error_duplicate_name')
		return
	}
	panic('expected error')
}

fn test_parse_list_option_result() {
	src := '
package p:q;

interface i {
	f: func(x: list<u8>, y: option<string>) -> result<s32, string>;
}
'
	doc := parse(src) or { panic(err) }
	assert doc.interfaces.len == 1
	f := doc.interfaces[0].items[0].func or { panic('no func') }
	assert f.params.len == 2
	assert f.results.len == 1
	assert f.results[0].str() == 'result<s32, string>'
}
