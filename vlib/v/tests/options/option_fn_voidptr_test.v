struct Foo {
mut:
	func ?fn (voidptr) ?bool = unsafe { nil }
}

fn callback(foo &Foo) ?bool {
	return foo.func? == callback
}

fn test_main() {
	t := Foo{
		func: callback
	}
	assert t.func? == callback
	call_fn := t.func?
	assert call_fn(&t)?

	mut a := Foo{}
	a.func = callback
	assert a.func? == callback
	call_fn2 := a.func?
	assert call_fn2(&a)?
}
