struct Foo {
mut:
	func ?fn (voidptr) bool = unsafe { nil }
}

fn callback(foo &Foo) bool {
	return foo.func != none
}

type OptFn = fn (&Foo) bool

fn test_main() {
	t := Foo{
		func: callback
	}
	assert t.func? == callback
	if t.func != none {
		assert t.func(&t)
	} else {
		assert false
	}

	a := ?OptFn(callback)
	if a != none {
		assert a(&t)
	} else {
		assert false
	}
}
