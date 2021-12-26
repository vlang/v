struct MyError {
	code int
	msg  string
}

fn foo() int | none | IError {
	return IError(MyError{})
}

fn test_string_optional_none() {
	x := foo()
	println(x)
	assert true
}
