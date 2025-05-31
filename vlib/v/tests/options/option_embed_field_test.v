struct Foo {
	optional_one ?string
}

struct Bar {
	foo Foo
}

fn test_main() {
	b := Bar{Foo{
		optional_one: 'hello world'
	}}
	println(b.foo.optional_one)
	x := b.foo.optional_one as string

	assert x == 'hello world'
	assert (b.foo.optional_one as string) == 'hello world'
}
