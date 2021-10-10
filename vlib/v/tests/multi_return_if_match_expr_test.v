fn ab(x bool) (int, int) {
	return match x {
		true { 1, 1 }
		else { 0, 0 }
	}
}

fn xy(x bool) (int, int) {
	return if x { 3, 3 } else { 2, 2 }
}

fn test_multi_return_if_match_expr() {
	a, b := ab(true)
	c, d := ab(false)
	x, y := xy(true)
	z, w := xy(false)
	assert a == 1
	assert b == 1
	assert c == 0
	assert d == 0
	assert x == 3
	assert y == 3
	assert z == 2
	assert w == 2
}
