struct Object {
	name  string
	value int
}

fn multireturner(n int, s string) (int, string) {
	return n + 1, s
}

fn test_assign_multi_expr_func() {
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
	}
	assert g == 1
	assert h == 'good'
}

fn test_assign_multi_expr() {
	// helpers
	val1 := 1
	val2 := 2

	// simple case for match
	a, b, c := match false {
		true { 1, 2, 3 }
		false { 4, 5, 6 }
	}
	assert a == 4
	assert b == 5
	assert c == 6

	// test with first value `literal`
	d, e, f := if true { 1, 'awesome', [13] } else { 0, 'bad', [0] }
	assert d == 1
	assert e == 'awesome'
	assert f == [13]

	// test with first value `literal expr` and statement
	awesome := 'awesome'
	g, h, i := if true { 1 + val1, awesome, [13] } else { int(0), 'bad', [0] }
	assert g == 2
	assert h == 'awesome'
	assert i == [13]

	// test with first value `.name`
	j, k, l := if true { val1, 'awesome', [13] } else { val2, 'bad', [0] }
	assert j == 1
	assert k == 'awesome'
	assert l == [13]

	// test with first value name and peek != .comma
	m, n, o := if true { val1 + 1, val1, val1 } else { val2, val2, val2 }
	assert m == val1 + 1
	assert n == val1
	assert o == val1

	// test practical complex expressions
	val3 := Object{
		name: 'initial'
		value: 19
	}
	mut q, mut r, mut s := if true {
		1 + 1, 'awe' + 'some', Object{
			...val3
			name: 'ok'
		}
	} else {
		0, '0', Object{}
	}
	assert q == 2
	assert r == 'awesome'
	assert s.name == 'ok'
	assert s.value == 19

	// test assign to existing variables
	q, r, s = if false {
		0, '0', Object{}
	} else {
		5, '55', Object{
			...val3
			value: 555
		}
	}
	assert q == 5
	assert r == '55'
	assert s.value == 555
	assert s.name == 'initial'
}

fn test_issue_9330() {
	arr := '0.1'.split('.')
	a0, a1 := arr[0], arr[1].int()
	assert a0 == '0'
	assert a1 == 1
	b0, b1 := arr[0].int(), arr[1]
	assert b0 == 0
	assert b1 == '1'
	c0, c1 := arr[0], arr[1]
	assert c0 == '0'
	assert c1 == '1'
	d0, d1 := arr[0].int(), arr[1].f64()
	assert d0 == 0
	assert d1 == 1.0
}
