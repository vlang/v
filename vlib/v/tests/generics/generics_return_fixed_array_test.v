fn init_type[T]() T {
	return T{}
}

fn test_generics_return_fixed_array() {
	r1 := init_type[[3]int]()
	println(r1)
	assert '${r1}' == '[0, 0, 0]'
}
