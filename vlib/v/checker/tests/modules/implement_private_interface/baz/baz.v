// sub module
module baz

interface Foo {
	a int
}

pub fn print_foo(f Foo) {
	println(f.a)
}
