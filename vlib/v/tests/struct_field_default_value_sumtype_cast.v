struct Foo {
	x int
}

struct Bar {}

type FooBar = Foo | Bar

struct Abc {
    foobar FooBar = Foo { x: 0 }
}

fn main() {
	x := Abc{}
	println(x)
}
