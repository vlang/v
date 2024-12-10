fn test_match_expr_with_string_fn() {
	a := 'hello'
	ret := match a {
		'hello' { 1 }
		'fn' { 2 }
		else { 0 }
	}
	println(ret)
	assert ret == 1
}
