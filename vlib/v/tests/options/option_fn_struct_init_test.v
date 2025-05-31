type Baz = Foo

@[heap]
struct Foo {
	a  int    = 123
	b  string = 'b'
	cb ?fn (&Foo)
}

@[heap]
struct Bar {
	a  int    = 321
	b  string = 'a'
	cb ?fn (&Baz)
}

fn g(a Foo) Foo {
	return a
}

fn t(a Bar) Foo {
	return g(a: 123, b: 'foo', cb: a.cb)
}

fn test_main() {
	a := Foo{
		cb: none
	}

	b := Foo{
		cb: fn (a &Foo) {
		}
	}

	w := Bar{
		cb: b.cb
	}

	z := Bar{
		cb: a.cb
	}

	t(a: 1, cb: a.cb)
	t(a: 2, cb: b.cb)
	t(a: 3, cb: w.cb)
	t(a: 4, cb: z.cb)
}
