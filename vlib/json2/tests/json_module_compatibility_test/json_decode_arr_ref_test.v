// vtest vflags: -w
import json2

struct Test {
	a string
}

fn test_main() {
	x := json2.decode[[]&Test]('[{"a":"a"}]') or { exit(1) }
	assert x[0].a == 'a'
}
