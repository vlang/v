struct Foo {
mut:
	func ?fn (voidptr) = unsafe { nil }
}

fn callback(foo &Foo) {
}

fn test_main() {
	t := Foo{
		func: callback
	}
	assert t.func? == callback

	mut a := Foo{}
	a.func = callback
	assert a.func? == callback
}
