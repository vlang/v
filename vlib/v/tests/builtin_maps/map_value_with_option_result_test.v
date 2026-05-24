import os

struct AdbDevice {
	opts map[string]?string
}

fn test_map_value_with_option_result() {
	// avoid warnings
	_ := os.max_path_len
	assert true
}

fn foo(arg map[string]?string) ?string {
	return arg['akey']
}

struct Foo {
	map2 map[string]?string
}

fn bar() {
	map1 := map[string]?string{}
}

fn baz(arg map[string]?string) ?string {
	return arg['akey']
}

fn test_map_value_with_option_or_block() {
	m := map[string]?int{}
	value := m['missing'] or { none }
	assert value == none
	assert m['missing'] or { none } == none
}

fn test_map_value_with_option_or_block_value_fallback() {
	mut m := map[string]?string{}
	m['present'] = ?string('xyz')
	m['none'] = ?string(none)
	missing := m['missing'] or { 'abc' }
	present := m['present'] or { 'abc' }
	none_value := m['none'] or { 'abc' }
	assert missing == 'abc'
	assert present == 'xyz'
	assert none_value == 'abc'
}
