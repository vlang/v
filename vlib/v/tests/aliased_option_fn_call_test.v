type OptStr = ?string

fn foo() OptStr {
	return 'abc'
}

fn test_aliased_option_fn_call() {
	ret := foo()?
	println(ret)
	assert ret == 'abc'
}
