struct Foo<T> {
	data []T
}

fn new_foo<T>(len int) &Foo<T> {
	return &Foo{
		data: []T{len: len}
	}
}

fn test_generics_return_reference_generics_struct() {
	f := new_foo<int>(4)
	println(f)
	assert f.data == [0, 0, 0, 0]
}
