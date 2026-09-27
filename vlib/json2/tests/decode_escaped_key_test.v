import json2

struct Inner {
mut:
	ok string
}

struct Outer {
mut:
	strings map[string]string
	inner   Inner
}

struct Base {
mut:
	id int
}

struct WithEmbed {
	Base
mut:
	name string
}

// https://github.com/vlang/v/issues/27811
fn test_escaped_struct_and_map_keys() {
	o := json2.decode[Outer](r'{"str\u0069ngs":{"o\u006b":"OK"},"\u0069nner":{"ok":"yes"}}')!
	assert o.strings['ok'] == 'OK'
	assert o.inner.ok == 'yes'
}

fn test_escaped_key_with_embed() {
	w := json2.decode[WithEmbed](r'{"\u0069d": 7, "nam\u0065": "x"}')!
	assert w.id == 7
	assert w.name == 'x'
}

fn test_escaped_map_keys() {
	m := json2.decode[map[string]string](r'{"\u006bey": "value", "a\"b\n": "c"}')!
	assert m['key'] == 'value'
	assert m['a"b\n'] == 'c'
	assert m.len == 2
}

fn test_unescaped_keys_still_match() {
	o := json2.decode[Outer]('{"strings":{"ok":"OK"},"inner":{"ok":"yes"}}')!
	assert o.strings['ok'] == 'OK'
	assert o.inner.ok == 'yes'
}

fn test_escaped_key_roundtrip() {
	original := {
		'a"b\n': 1
	}
	assert json2.decode[map[string]int](json2.encode(original))! == original
}
