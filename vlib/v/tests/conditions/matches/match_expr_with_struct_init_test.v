struct Test {
	a string
}

fn test_match_expr_with_struct_init() {
	a := map[string]string{}
	b := match 'test' {
		'test' {
			Test{a['test'] or { '' }}
		}
		else {
			Test{''}
		}
	}
	println(b)
	assert b.a == ''
}
