struct Am {
	inner int
}

fn convert[T](num int) T {
	return T{num}
}

fn test_generic_struct_no_key_init() {
	println(convert[Am](3).inner)
	assert convert[Am](3).inner == 3
}
