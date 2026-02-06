fn test_if_expr_with_nested_match_expr() {
	a := if true {
		match `a` {
			`a` { 0 }
			else { 1 }
		}
	} else {
		3
	}
	println(a)
	assert a == 0
}
