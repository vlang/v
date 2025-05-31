struct Foo {
}

struct Bar {
	do fn () bool = unsafe { nil }
}

fn (f Foo) get() Bar {
	return Bar{
		do: foobar
	}
}

fn foobar() bool {
	return true
}

fn test_main() {
	t := Foo{}
	assert t.get().do() == true
}
