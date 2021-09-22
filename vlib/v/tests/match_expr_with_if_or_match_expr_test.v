fn foo1() int {
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

fn foo2() int {
	return match 1 {
		1 {
			match true {
				true { 1 }
				false { 2 }
			}
		}
		else {
			match false {
				true { 3 }
				false { 4 }
			}
		}
	}
}

fn test_match_expr_with_if_or_match_expr() {
	println(foo1())
	assert foo1() == 1

	println(foo2())
	assert foo2() == 1
}
