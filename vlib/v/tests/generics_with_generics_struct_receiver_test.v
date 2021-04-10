struct Num<T> {
	num T
}

fn (num Num<T>) is_autom<T>() bool {
	return true
}

fn test_generics_with_generic_struct_receiver() {
	num := Num<int>{3}
	println(num.is_autom())
	assert num.is_autom()
}
