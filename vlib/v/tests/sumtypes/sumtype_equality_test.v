type Str = rune | string

struct Foo {
	v int
}

struct Bar {
	v int
}

type FooBar = Bar | Foo

fn test_sumtype_equality() {
	s1 := Str('s')
	s2 := Str('s2')
	u1 := Str(`A`)
	u2 := Str(`B`)
	assert s1 == s1
	assert u1 == u1
	assert s1 != s2
	assert u1 != u2
	assert u1 != s1

	// Same value, different type
	foo := FooBar(Foo{
		v: 0
	})
	bar := FooBar(Bar{
		v: 0
	})
	assert foo.v == bar.v
	assert foo != bar
}
