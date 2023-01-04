struct Foo {
mut:
	x int
}

struct Bar {
	Foo
}

struct Baz {
	Bar
}

fn test_nested_struct_embed() {
	mut baz := Baz{}
	baz.x = 3

	println(baz.x)
	assert baz.x == 3
}
