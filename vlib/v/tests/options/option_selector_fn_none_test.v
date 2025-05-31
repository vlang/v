struct Foo {
mut:
	func ?fn (voidptr) ?bool
}

fn callback(foo &Foo) ?bool {
	return foo.func? == callback
}

fn test_main() {
	t := Foo{}
	assert callback(&t) == none
}
