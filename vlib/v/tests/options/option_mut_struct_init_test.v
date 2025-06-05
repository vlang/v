struct Bar {
mut:
	a int
}

struct Foo {
	field ?&Bar
}

fn t(mut opt ?Bar) {
	v := Foo{
		field: opt
	}
	if opt == none {
		assert opt == none
		assert v.field == none
	} else {
		assert opt.a == 123
		assert v.field != none
	}
	if mut opt != none {
		opt.a = 321
	}
}

fn test_main() {
	mut var := ?&Bar(none)
	t(mut var)
	assert var == none
}

fn test_not_none() {
	mut var := ?&Bar(&Bar{
		a: 123
	})
	t(mut var)
	assert var != none
	assert var?.a == 321
}
