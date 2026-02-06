import json

@[heap]
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

	assert dump(*foo.a) == 123
	assert dump(*foo.b) == 'foo'
	assert dump(*foo.c) == 1.2
	assert dump(**foo.d) == 321
	assert dump(***foo.e) == 'bar'

	assert dump(json.encode(foo)) == '{"a":123,"b":"foo","c":1.2,"d":321,"e":"bar"}'
}

fn test_option_ptr() ? {
	data := '{ "a": 123, "b": "foo", "c": 1.2, "d": 321, "e": "bar"}'
	foo := json.decode(FooOption, data) or { return none }
	println(foo)

	assert dump(*foo.a?) == 123
	assert dump(*foo.b?) == 'foo'
	assert dump(*foo.c?) == 1.2
	assert dump(**foo.d?) == 321
	assert dump(***foo.e?) == 'bar'

	assert dump(json.encode(foo)) == '{"a":123,"b":"foo","c":1.2,"d":321,"e":"bar"}'
}
