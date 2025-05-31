fn foo() {
}

fn test_print_reference_function() {
	println(&foo)
	assert '${&foo}' == 'fn ()'
}
