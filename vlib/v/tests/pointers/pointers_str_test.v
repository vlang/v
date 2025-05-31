struct Aaa {
	foo int
}

fn test_pointer_to_string() {
	a := Aaa{}
	assert a.foo.str() != (&a.foo).str()
}
