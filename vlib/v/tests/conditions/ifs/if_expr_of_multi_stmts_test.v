fn test_if_expr_of_multi_stmts() {
	a := 2
	ret := if a > 1 {
		mut b := 1
		b *= 10
		println(b)
		b
	} else {
		mut c := 0
		c += 2
		println(c)
		c
	}
	assert ret == 10
}
