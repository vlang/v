// vtest vflags: -w
import json2

@[heap]
struct Foo {
	a &int
	b &string
	c &f64
}

struct FooOption {
mut:
	a ?&int
	b ?&string
	c ?&f64
}

// json2 encodes pointers to pointers, but decodes only single pointers.
struct FooMulti {
	d &&int
	e &&&string
}

struct FooMultiOption {
mut:
	d ?&&int
	e ?&&&string
}

fn test_ptr() {
	data := '{ "a": 123, "b": "foo", "c": 1.2}'
	foo := json2.decode[Foo](data)!
	println(foo)

	assert dump(*foo.a) == 123
	assert dump(*foo.b) == 'foo'
	assert dump(*foo.c) == 1.2

	assert dump(json2.encode(foo, escape_unicode: true)) == '{"a":123,"b":"foo","c":1.2}'
}

fn test_option_ptr() ? {
	data := '{ "a": 123, "b": "foo", "c": 1.2}'
	foo := json2.decode[FooOption](data) or { return none }
	println(foo)

	assert dump(*foo.a?) == 123
	assert dump(*foo.b?) == 'foo'
	assert dump(*foo.c?) == 1.2

	assert dump(json2.encode(foo, escape_unicode: true)) == '{"a":123,"b":"foo","c":1.2}'
}

fn test_ptr_ptr_encode() {
	d := 321
	d_ptr := &d
	e := 'bar'
	e_ptr := &e
	e_ptr_ptr := &e_ptr
	foo := FooMulti{
		d: &d_ptr
		e: &e_ptr_ptr
	}
	assert **foo.d == 321
	assert ***foo.e == 'bar'
	assert json2.encode(foo, escape_unicode: true) == '{"d":321,"e":"bar"}'

	// A comptime condition on the substituted `?&&int` type used to be split at its
	// `&&` like a logical AND, leaving the whole `$if` unresolved.
	foo_option := FooMultiOption{
		d: &d_ptr
		e: &e_ptr_ptr
	}
	assert json2.encode(foo_option, escape_unicode: true) == '{"d":321,"e":"bar"}'
}
