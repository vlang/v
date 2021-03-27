struct Foo {
	x int
}

interface FooBar {
	x int
}

struct Abc {
	foobar FooBar = Foo{
		x: 123
	}
}

fn test_struct_field_default_value_interface_cast() {
	x := Abc{}
	assert x.foobar is Foo
	assert x.foobar.x == 123
}
