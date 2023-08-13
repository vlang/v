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

fn foo() IError {
	return MyError{}
}

fn test_string_option_none() {
	x := foo()
	println(x)
	assert true
}
