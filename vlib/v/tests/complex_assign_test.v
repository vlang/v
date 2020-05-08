
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

	// TODO: returning non-function calls does not work yet due to parsing issues
	/*
	e, f := if true {
		1, 'awesome'
	} else {
		0, 'bad'
	}
	*/
}
