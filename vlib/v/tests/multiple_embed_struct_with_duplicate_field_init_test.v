struct Foo {
mut:
	x int
}

struct Bar {
	Foo
mut:
	x int
}

fn test_multiple_embed_struct_with_duplicate_field_init() {
	mut b := Bar{
		x: 2
	}
	println(b)
	assert b.x == 2
	assert b.Foo.x == 0
}
