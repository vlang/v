struct Foo {
	abc int
}

fn test_print_address_of_reference_struct() {
	foo := Foo{}
	foo_ptr := &foo
	println('${foo_ptr:p}')
	println('${&foo:p}')
	assert true
}
