struct MyError {
	msg  string
	code int
}

fn (err MyError) msg() string {
	return err.msg
}

fn (err MyError) code() int {
	return err.code
}

fn foo() int | none | IError {
	return IError(MyError{})
}

fn test_string_optional_none() {
	x := foo()
	println(x)
	assert true
}
