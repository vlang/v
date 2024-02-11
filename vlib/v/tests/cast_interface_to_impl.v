interface Foo {
	f() int
}

struct Bar {}

fn (_ Bar) f() int {
	return 2
}

fn (_ Bar) secret1() int {
	return 42
}

struct Baz {}

fn (_ Baz) f() int {
	return 8
}

fn (_ Baz) secret2() int {
	return 84
}

fn h(foo Foo, i int) int {
	j := foo.f()
	match i {
		0 {
			return j + (foo as Bar).secret1()
		}
		1 {
			return j + (foo as Baz).secret2()
		}
		else {
			return 1
		}
	}
}

fn test_casting_to_impl() {
	assert h(Bar{}, 0) == 44
	assert h(Baz{}, 1) == 92
}
