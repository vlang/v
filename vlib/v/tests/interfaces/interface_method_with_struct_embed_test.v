struct Foo {
	x int
}

fn (f Foo) get() int {
	return f.x
}

struct Bar {
	Foo
}

interface Getter {
	get() int
}

fn test_interface_method_with_struct_embed() {
	mut getter := []Getter{}

	foo := Foo{22}
	getter << foo

	bar := Bar{Foo{11}}
	getter << bar

	assert getter.len == 2

	println(foo)
	println(getter[0])
	assert getter[0].get() == 22

	println(bar)
	println(getter[1])
	assert getter[1].get() == 11
}
