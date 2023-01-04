fn test_or_expr_with_multi_stmts() {
	x := fmt_test() or {
		println(err.msg())
		-100
	}
	println(x)
	assert x == -100
}

fn fmt_test() ?int {
	return error('foo')
}
