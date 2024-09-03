fn cast_interface[T, U](u U) T {
	$if U is $interface {
		if u is T {
			return u
		} else {
			panic('expected t to be ${typeof[T]().name}, got ${typeof[U]().name}')
		}
	} $else {
		$compile_error('not an interface')
	}
}

interface Foo {
	f()
}

struct Bar {}

fn (bar Bar) f() {}

struct Baz {}

fn (baz Baz) f() {}

fn (_ Bar) g() int {
	return 0
}

fn (_ Baz) g() int {
	return 1
}

fn f(foo Foo) int {
	if foo is Bar {
		return cast_interface[Bar, Foo](foo).g()
	}
	if foo is Baz {
		return cast_interface[Baz, Foo](foo).g()
	}
	return -1
}

fn test_main() {
	assert f(Bar{}) == 0
	assert f(Baz{}) == 1
}
