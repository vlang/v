module main

import zmod_foo { Foo }

struct Bar {}

fn main() {
	// note: will succeed with foo.Foo
	f := Foo.new[Bar](Bar{})

	println(typeof(f).name)
}
