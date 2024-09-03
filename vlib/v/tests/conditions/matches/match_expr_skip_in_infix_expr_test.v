fn test_match_expr_skip_in_infix_expr_1() {
	mut a := false
	b := a && match true {
		true {
			a = true
			true
		}
		false {
			false
		}
	}
	println(a)
	assert a == false
	println(b)
	assert b == false
}

fn test_match_expr_skip_in_infix_expr_2() {
	mut a := true
	b := a || match true {
		true {
			a = false
			true
		}
		false {
			false
		}
	}
	println(a)
	assert a == true
	println(b)
	assert b == true
}
