struct Bar {
	a int
}

struct Foo {
	field ?&Bar
}

fn t(mut opt ?Bar) {
	_ := Foo{
		field: opt
	}
	assert opt == none
}

fn test_main() {
	mut var := ?&Bar(none)
	t(mut var)
	assert var == none
}
