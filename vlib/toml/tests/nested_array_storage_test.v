import json2
import toml
import toml.to

fn test_path_keys_survive_parser_disposal() {
	document := toml.parse_text('root."nested.key".value = 7\n'.clone())!
	gc_collect()
	expected := '{"root":{"nested.key":{"value":7}}}'
	actual := json2.encode(to.json_any(document.to_any()))
	assert actual == expected
}

fn test_nested_array_paths_preserve_table_values() {
	inputs := [
		'[[a]]\n[[a.b]]\nx=1\n[[a]]\n[a.b]\nx=2\n',
		'a=[{c={d=1}},{c={d=2}}]\n[[c.e]]\nvalue=3\n',
		'[[a]]\nc={d=1}\n[[c.e]]\nvalue=2\n',
		'[[a]]\nc={d=1}\n[[a]]\n[a.c]\ne=2\n',
		'[[a.b]]\nc={d=1}\n[[c.e]]\nvalue=2\n',
		'[[a.b]]\nc={d=1}\n[[a.b]]\n[a.b.c]\ne=2\n',
		'[[a]]\nc={d={e=1}}\n[[a]]\n[[a.c.d]]\ne=2\n',
		'[[a.b.c]]\nx=1\n[[a.b.c]]\nx=2\n',
		'[[a]]\nid=1\n[[a.b.c]]\nx=1\n[[a]]\nid=2\n[[a.b.c]]\nx=2\n',
		'[["a.b".c.d]]\nx=1\n',
		'[[a.b]]\ntrue=1\nfalse=2\n',
		'[[a.b.c]]\nx=1\n[a.b.c.meta]\ny=2\n[[a.b.c]]\nx=3\n',
		'[[a.b]]\nid=1\n[[a.b.c]]\nx=1\n[[a.b]]\nid=2\n[[a.b.c]]\nx=2\n',
	]
	expected := [
		'{"a":[{"b":[{"x":1}]},{"b":{"x":2}}]}',
		'{"a":[{"c":{"d":1}},{"c":{"d":2}}],"c":{"e":[{"value":3}]}}',
		'{"a":[{"c":{"d":1}}],"c":{"e":[{"value":2}]}}',
		'{"a":[{"c":{"d":1}},{"c":{"e":2}}]}',
		'{"a":{"b":[{"c":{"d":1}}]},"c":{"e":[{"value":2}]}}',
		'{"a":{"b":[{"c":{"d":1}},{"c":{"e":2}}]}}',
		'{"a":[{"c":{"d":{"e":1}}},{"c":{"d":[{"e":2}]}}]}',
		'{"a":{"b":{"c":[{"x":1},{"x":2}]}}}',
		'{"a":[{"id":1,"b":{"c":[{"x":1}]}},' +
			'{"id":2,"b":{"c":[{"x":2}]}}]}',
		'{"a.b":{"c":{"d":[{"x":1}]}}}',
		'{"a":{"b":[{"true":1,"false":2}]}}',
		'{"a":{"b":{"c":[{"x":1,"meta":{"y":2}},{"x":3}]}}}',
		'{"a":{"b":[{"id":1,"c":[{"x":1}]},' +
			'{"id":2,"c":[{"x":2}]}]}}',
	]
	for i, input in inputs {
		document := toml.parse_text(input)!
		actual := json2.decode[json2.Any](to.json(document))!
		want := json2.decode[json2.Any](expected[i])!
		assert actual == want, input
	}
}

fn test_nested_array_headers_reject_immutable_parents() {
	inputs := [
		'[[a]]\nc={d=1}\n[[a.c.e]]\nvalue=2\n',
		'[[a.b]]\nc={d=1}\n[[a.b.c.e]]\nvalue=2\n',
		'[[a.b]]\nc={d=1}\n[a.b.c]\ne=2\n',
		'a = {b={}}\n[[a.b.c]]\nx=1\n',
		'a.b = {c=[]}\n[[a.b.c]]\nx=1\n',
		'a = [{x=1}]\n[[a.b]]\ny=2\n',
		'a.b = [{x=1}]\n[a.b.c]\ny=2\n',
	]
	for input in inputs {
		if _ := toml.parse_text(input) {
			assert false, input
		}
	}
}

fn test_array_table_depth_is_not_limited_to_two_components() {
	parts := []string{len: 24, init: 'level${index}'}
	path := parts.join('.')
	document := toml.parse_text('[[${path}]]\nvalue=7\n')!
	entries := document.value(path).array()
	assert entries.len == 1
	value := entries[0].as_map()['value'] or { panic('missing value') }
	assert value.int() == 7
}
