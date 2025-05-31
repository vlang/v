struct Foo {
mut:
	f ?fn (int)
	g fn (int)
}

fn test_main() {
	mut foo := ?Foo{}
	assert foo == none

	foo = Foo{}
	assert foo != none

	println(foo?.f)
	println('${foo?.f}')
}
