struct Foo {
	x int
}

struct Bar {}

type FooBar = Foo | Bar

struct Abc {
    foobar FooBar = Foo { x: 123 }
}

fn test_struct_field_default_value_sumtype_cast() {
	x := Abc{}
	assert x.foobar is Foo
	assert (x.foobar as Foo).x == 123
}
