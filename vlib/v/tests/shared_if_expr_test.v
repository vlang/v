type AA = bool | int

fn test_shared_if_expr() {
	shared a := [1, 2, 3]
	b := [4, 5, 6]
	c := lock a {
		if a == b { a } else { b }
	}
	assert c == [4, 5, 6]
	d := lock a {
		if a != b {
			a << 5
			a
		} else {
			b
		}
	}
	assert d == [1, 2, 3, 5]
}
