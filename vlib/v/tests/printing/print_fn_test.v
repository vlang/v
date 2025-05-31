struct Foo {
mut:
	f ?fn (int)
}

fn test_print_fn() {
	a := fn (a string) int {
		return 1
	}
	println(a)

	foo := Foo{}
	println(foo.f)
	assert foo.f == none
}
