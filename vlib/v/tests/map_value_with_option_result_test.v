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
