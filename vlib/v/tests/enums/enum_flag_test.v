@[flag]
enum Foo {
	a
	b
	c
}

const a = Foo.a

const ab = Foo.a | Foo.b

const ab2 = Foo.a | .b

const abc = Foo.a | Foo.b | Foo.c

const abc2 = Foo.a | .b | .c

const abc3 = Foo.a ^ .b

const abc4 = ~Foo.a

fn test_main() {
	assert dump(a) == Foo.a
	assert dump(ab) == Foo.a | Foo.b
	assert dump(ab2) == Foo.a | .b
	assert dump(abc) == Foo.a | Foo.b | Foo.c
	assert dump(abc2) == Foo.a | .b | .c
	assert dump(abc3) == Foo.a ^ .b
	assert dump(abc4) == ~Foo.a
}
