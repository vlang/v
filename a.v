struct Foo {
	foo int
}

type Test = struct {
	foo int
}

fn main() {
	test := Test(Foo{foo: 0})	
}
