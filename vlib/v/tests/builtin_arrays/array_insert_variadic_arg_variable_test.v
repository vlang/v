fn foo(args ...string) []string {
	mut v := ['a']
	v.insert(1, ['b'])
	v.insert(1, args)
	return v
}

fn test_array_insert_variadic_arg_variable() {
	ret := foo('b')
	println(ret)
	assert ret == ['a', 'b', 'b']
}
