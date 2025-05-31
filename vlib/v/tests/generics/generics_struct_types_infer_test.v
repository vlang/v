struct Foo[T, U] {
	a T
	b U
}

fn test_generic_struct_types_infer() {
	st11 := Foo{'two', 2}
	println(st11.a)
	println(st11.b)
	assert st11.a == 'two'
	assert st11.b == 2

	st12 := Foo{
		a: 'two'
		b: 2
	}
	println(st12.a)
	println(st12.b)
	assert st12.a == 'two'
	assert st12.b == 2

	st21 := Foo{1, 'one'}
	println(st21.a)
	println(st21.b)
	assert st21.a == 1
	assert st21.b == 'one'

	st22 := Foo{
		a: 1
		b: 'one'
	}
	println(st22.a)
	println(st22.b)
	assert st22.a == 1
	assert st22.b == 'one'
}
