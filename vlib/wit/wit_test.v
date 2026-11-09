module wit

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
