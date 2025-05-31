struct Foo {
	foo ?string
}

fn test_struct_option_field_zero() {
	a := Foo{}
	if foo := a.foo {
		println(foo)
		assert false
	} else {
		assert true
	}

	b := Foo{'hello'}
	if foo := b.foo {
		println(foo)
		assert true
	} else {
		assert false
	}
}
