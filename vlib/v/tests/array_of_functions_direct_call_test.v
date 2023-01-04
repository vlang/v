fn foo() string {
	println('foo')
	return 'foo'
}

fn bar() string {
	println('bar')
	return 'bar'
}

fn call() {
	funcs := [foo, bar]
	i := 1
	ret := funcs[i]()
	assert ret == 'bar'
}

fn test_array_of_functions_direct_call() {
	call()
}
