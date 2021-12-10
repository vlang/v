fn foo(a string) int {
	return 10 + a.len
}

fn test_fixed_array_fn_index() {
	a := [foo]!
	println(a[0]('hello'))
	assert a[0]('hello') == 15
}
