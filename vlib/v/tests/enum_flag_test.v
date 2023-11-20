@[flag]
enum Foo {
	a
	b
	c
}

const a = Foo.a

const ab = Foo.a | Foo.b

const abc = Foo.a | Foo.b | Foo.c

fn test_main() {
	assert dump(a) == Foo.a
	assert dump(ab) == Foo.a | Foo.b
	assert dump(abc) == Foo.a | Foo.b | Foo.c
}
