type Ty = bool | int

fn test(a bool) Ty {
	return match a {
		true { 123 }
		else { false }
	}
}

fn test_match_expr_sumtype_eval() {
	assert test(true) == Ty(123)
	assert test(false) == Ty(false)
}
