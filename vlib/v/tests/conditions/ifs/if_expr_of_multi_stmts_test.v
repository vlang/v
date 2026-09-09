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

fn test_if_expr_of_multi_stmts_returning_anon_fn() {
	condition := true
	f := if condition {
		println('f')
		fn (value string) string {
			return 'left:${value}'
		}
	} else {
		fn (value string) string {
			return 'right:${value}'
		}
	}
	assert f('x') == 'left:x'
}
