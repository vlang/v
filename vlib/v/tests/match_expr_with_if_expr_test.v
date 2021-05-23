fn foo() int {
	return match 1 {
		1 {
			if true {
				1
			} else {
				2
			}
		}
		else {
			if true {
				3
			} else {
				4
			}
		}
	}
}

fn test_match_expr_with_if_expr() {
	println(foo())
	assert foo() == 1
}
