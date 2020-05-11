
fn multireturner(n int, s string) (int, string) {
	return n + 1, s
}

fn test_assign_multireturn_expression() {
	a, b := if true {
		multireturner(13, 'awesome')
	} else {
		multireturner(-1, 'notawesome')
	}
	assert a == 14
	assert b == 'awesome'

	c, d := if false {
		multireturner(-1, 'notawesome')
	} else if true {
		multireturner(17, 'awesomer')
	} else {
		multireturner(-1, 'notawesome')
	}
	assert c == 18
	assert d == 'awesomer'

	e, f := if false {
		multireturner(-1, 'notawesome')
	} else if false {
		multireturner(-1, 'notawesome')
	} else {
		multireturner(17, 'awesomer')
	}
	assert e == 18
	assert f == 'awesomer'

	g, h := match true {
		true { multireturner(0, 'good') }
		false { multireturner(100, 'bad') }
		else { multireturner(200, 'bad') }
	}
	assert g == 1
	assert h == 'good'

	i, j := match true {
		false { multireturner(100, 'bad') }
		else { multireturner(0, 'good') }
	}
	assert i == 1
	assert j == 'good'

	k, l, m := if true {
		1, 'awesome', [13]
	} else {
		0, 'bad', [0]
	}
	assert k == 1
	assert l == 'awesome'
	assert m == [13]

	n, o, p := if false {
		1, 'awesome', [13]
	} else {
		0, 'bad', [0]
	}
	assert n == 0
	assert o == 'bad'
	assert p == [0]

	static var1 := 17
	mut var2 := 'awe'
	var3 := [13]
	q, r, s := if true {
		// if-expr can not contain var-decl
		var2 += 'some'
		var1, var2, var3
	} else {
		0, 'bad', [0]
	}
	assert q == 17
	assert r == 'awesome'
	assert s == [13]
}
