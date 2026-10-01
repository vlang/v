// vtest vflags: -w
import json2

struct Foo {
	x   int = 123
	bar int = -112233 @[skip]
	y   int = 456
}

fn test_check_field_default_expr() {
	f := Foo{}
	// eprintln('f: ${f}')
	assert f.bar == -112233
	assert f.x == 123
	assert f.y == 456
}

fn test_check_field_skip_attribute() {
	f := Foo{}
	s := json2.encode(f, escape_unicode: true)
	// eprintln('f: ${f}')
	// eprintln('s: ${s}')
	assert s == '{"x":123,"y":456}'
}
