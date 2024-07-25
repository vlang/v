struct Foo {
	bar &Bar = Bar{}
}

struct Bar {
	field int
}

fn test_main() {
	mut b := Bar{
		field: 10
	}
	mut a := Foo{
		bar: &b
	}
	assert dump(a.bar) == b

	x := dump(Foo{})
	assert x.bar.field == 0
}
