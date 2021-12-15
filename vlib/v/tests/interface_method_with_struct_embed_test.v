interface Getter {
	get() int
}

struct Foo {
	x int
}

fn (f Foo) get() int {
	return f.x
}

struct Bar {
	Foo
}

struct Baz {
	Foo
}

fn (b Baz) get() int {
	return 42
}

fn test_interface_method_with_struct_embed() {
	foo := Foo{11}
	bar := Bar{Foo{22}}
	baz := Baz{Foo{33}}

	assert Getter(foo).get() == 11
	assert Getter(bar).get() == 22
	assert Getter(baz).get() == 42
}
