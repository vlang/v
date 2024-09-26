fn test_match_expr_in_infix_expr() {
	mut a := true
	b := a && match true {
		true {
			a = false
			true
		}
		false {
			false
		}
	}
	println(a)
	assert a == false
	println(b)
	assert b == true
}
