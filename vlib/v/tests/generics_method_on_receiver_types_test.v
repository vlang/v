struct Abc<T> {
	value T
}

fn (s Abc<T>) get_value() T {
	return s.value
}

fn test_generics_method_on_receiver_types() {
	s := Abc<string>{'hello'}
	println(s.get_value())
	assert s.get_value() == 'hello'
}
