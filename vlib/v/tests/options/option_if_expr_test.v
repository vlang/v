fn f() ?int {
	return none
}

fn test_option_if_expr() {
	fallback := 0
	i := f() or {
		if fallback == 0 {
			int(0)
		} else {
			int(-1)
		}
	}
	println(i)
	assert i == 0
}
