struct Err {
	msg  string
	code int
}

fn (e Err) msg() string {
	return e.msg
}

fn (e Err) code() int {
	return e.code
}

fn foo() ?string {
	return 'foo'
}

fn bar() !(string, int) {
	a := foo() or { return Err{
		msg: 'error test'
	} }
	return a, 1
}

fn test_results_multi_return() {
	b, _ := bar() or { panic(err) }
	println(b)
	assert b == 'foo'
}
