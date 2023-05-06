import json

[heap]
struct Foo {
	a &int
	b &string
	c &f64
	d &&int
	e &&&string
}

struct FooOption {
mut:
	a ?&int
	b ?&string
	c ?&f64
	d ?&&int
	e ?&&&string
}

fn test_ptr() {
	data := '{ "a": 123, "b": "foo", "c": 1.2, "d": 321, "e": "bar"}'
	foo := json.decode(Foo, data)!
	println(foo)

	dump(*foo.a)
	dump(*foo.b)
	dump(*foo.c)
	dump(**foo.d)
	dump(***foo.e)
}

fn test_option_ptr() ? {
	data := '{ "a": 123, "b": "foo", "c": 1.2, "d": 321, "e": "bar"}'
	foo := json.decode(FooOption, data) or { return none }
	println(foo)

	dump(*foo.a?)
	dump(*foo.b?)
	dump(*foo.c?)
	dump(**foo.d?)
	dump(***foo.e?)
}
