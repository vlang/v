fn test_if_expr_with_multi_array_call() {
	ret := foo()
	println(ret)
	assert ret == 'all'
}

fn foo() string {
	x := [3, 4]
	y := [2, 3, 4, 5]
	if x.all(it in y) || y.all(it in x) {
		return 'all'
	} else if x.any(it in y) {
		return 'any'
	}
	return ''
}
