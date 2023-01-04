struct Qwe {
mut:
	a f64
}

fn f() &Qwe {
	q := Qwe{
		a: 12.5
	}
	return &q
}

fn g() f64 {
	a := -0.125
	b := 3
	c := a * b
	return c
}

fn test_reference_return() {
	x := f()
	y := g()
	assert x.a == 12.5
	assert y == -0.375
}
