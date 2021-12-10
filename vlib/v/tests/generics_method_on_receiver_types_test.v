struct Abc<T> {
	value T
}

fn (s Abc<T>) get_value() T {
	return s.value
}

fn test_generics_method_on_receiver_types() {
	s1 := Abc<string>{'hello'}
	println(s1.get_value())
	assert s1.get_value() == 'hello'

	s2 := Abc<int>{22}
	println(s2.get_value())
	assert s2.get_value() == 22

	s3 := Abc<f64>{1.1}
	println(s3.get_value())
	assert s3.get_value() == 1.1

	s4 := Abc<bool>{true}
	println(s4.get_value())
	assert s4.get_value() == true
}
