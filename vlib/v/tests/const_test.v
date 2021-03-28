pub const (
	a = b
	c = a + b
	b = 1
	d = (e / 2) + 7
	e = 9
)

pub const x = 10

fn test_const() {
	assert a == 1
	assert d == 11
	assert c == 1
}

// const optional test
struct Foo {
	name string = 'foo'
}

fn foo_decode(name string) ?Foo {
	if name == 'baz' {
		return error('baz is not allowed')
	}
	return Foo{name}
}

pub const (
	def = foo_decode('baz') or { Foo{} }
	bar = foo_decode('bar') ?
)

fn test_opt_const() {
	assert def.name == 'foo'
	assert bar.name == 'bar'
}
