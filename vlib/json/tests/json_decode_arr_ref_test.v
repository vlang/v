import json

struct Test {
	a string
}

fn test_main() {
	x := json.decode([]&Test, '[{"a":"a"}]') or { exit(1) }
	assert x[0].a == 'a'
}
