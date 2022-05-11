pub const (
	a   = b
	ccc = a + b
	b   = 1
	d   = (e / 2) + 7
	e   = 9
)

pub const x = 10

fn test_const() {
	assert d == 11
	//
	assert b == 1
	assert a == 1
	assert ccc == a + b
	assert e == 9
	assert d == (e / 2) + 7
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
	bar = foo_decode('bar')?
)

fn test_opt_const() {
	assert def.name == 'foo'
	assert bar.name == 'bar'
}

// const with expressions that compile to multiple C statements
pub const (
	abc = [1, 2, 3].map(it * it)
	ghi = [1, 2, 3, 4, 5].filter(it % 2 == 0)
	jkl = [`a`, `b`, `c`].contains(`d`)
)

fn test_multistmt_const() {
	assert abc[2] == 9
	assert ghi.len == 2
	assert jkl == false
}
