struct Object {
	name string
	value int
}

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

	mut var1 := 17
	var2 := 'awesome'
	q, r, s := if true {
		1 + var1, var2, [13]
	} else {
		0, 'bad', [0]
	}
	assert q == 18
	assert r == 'awesome'
	assert s == [13]

	val1 := 1
	val2 := 0
	t, u, v := if true {
		val1, 'awesome', [13]
	} else {
		val2, 'bad', [0]
	}
	assert t == val1
	assert u == 'awesome'
	assert v == [13]

	val3 := Object { name: 'foo', value: 19 }
	x, y, z := if true {
		1 + 1, 'awe' + 'some', { val3 | name: 'bar' }
	} else {
		0, '0', Object {}
	}
	assert x == 2
	assert y == 'awesome'
	assert z.name == 'bar'
	assert z.value == 19

}
