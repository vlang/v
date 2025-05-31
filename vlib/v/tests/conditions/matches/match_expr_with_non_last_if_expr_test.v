fn test_match_expr_with_non_last_if_expr() {
	out := match true {
		true {
			{
			}
			if true {
			} else {
			}
			'a'
		}
		else {
			for {
				break
			}
			'b'
		}
	}
	println(out)
	assert out == 'a'
}
