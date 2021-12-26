struct MyError {
	code int
	msg  string
}

fn foo() int | none | IError {
	return none
}

fn test_string_optional_none() {
	x := foo()
	println(x)
	assert '$x'.contains('(none)')
}
