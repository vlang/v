struct Foo {
	x int
}

interface FooBar {
	x int
}

struct Abc {
    foobar FooBar = Foo { x: 123 }
}

fn main() {
	x := Abc{}
	println(123)
	assert x.foobar is Foo
	assert x.foobar.x == 123
}
