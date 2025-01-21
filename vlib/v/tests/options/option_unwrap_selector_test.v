interface IBar {
	opt ?string
}

struct Bar implements IBar {
	opt ?string
}

struct Foo {
	field IBar
}

fn (f &Foo) t() {
	if f.field.opt != none {
		assert f.field.opt == 'foo'
	}
}

fn test_main() {
	a := Foo{
		field: Bar{
			opt: 'foo'
		}
	}
	if a.field.opt != none {
		assert a.field.opt == 'foo'
	}
	a.t()
}
