module main

import baz

fn main() {
	b := Bar{10}
	baz.print_foo(b) // prints 10
}

struct Bar {
	a int
}
