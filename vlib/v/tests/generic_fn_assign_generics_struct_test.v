struct Test<T> {
	v T
}

fn get_test<T>(v T) Test<T> {
	return Test{
		v: v
	}
}

fn test_generics_assign_generics_struct() {
	x := get_test(1)
	println('$x.v')
	assert x.v == 1
}
