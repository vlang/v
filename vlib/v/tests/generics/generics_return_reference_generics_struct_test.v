struct Foo[T] {
	data []T
}

fn new_foo[T](len int) &Foo[T] {
	return &Foo[T]{
		data: []T{len: len}
	}
}

fn test_generics_return_reference_generics_struct() {
	f1 := new_foo[int](4)
	println(f1)
	assert f1.data == [0, 0, 0, 0]

	f2 := new_foo[bool](4)
	println(f2)
	assert f2.data == [false, false, false, false]

	f3 := new_foo[f64](4)
	println(f3)
	assert f3.data == [0.0, 0.0, 0.0, 0.0]
}
