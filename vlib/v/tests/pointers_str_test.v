struct A {
	foo int
}

fn test_pointer_to_string() {
	a := A{}
	assert a.foo.str() != (&a.foo).str()
}
