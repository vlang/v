struct Foo {
	a int
}

fn Foo.init[T](a T) {
	dump(a)
}

@[heap]
struct Bar {
	field int
}

fn t[T](a ?T) T {
	mut b := a
	if b != none {
		Foo.init(b)
		return b
	}
	assert false
	return T{}
}

fn test_main() {
	assert t(Bar{}) == Bar{}
}
