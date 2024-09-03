fn f() ?int {
	return none
}

fn test_option_if_expr() {
	i := f() or {
		if err is none {
			int(0)
		} else {
			eprintln(err)
			int(-1)
		}
	}
	println(i)
	assert i == 0
}
