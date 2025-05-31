struct Foo {
mut:
	foo string
}

struct Bar {
	Foo
}

fn test_main() {
	shared bar := Bar{}
	rlock bar {
		assert bar.foo == ''
	}
}
