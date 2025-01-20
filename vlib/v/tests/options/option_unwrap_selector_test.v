interface IBar {
	opt ?string
}

struct Bar implements IBar {
	opt ?string
}

struct Foo {
	field IBar
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
}
