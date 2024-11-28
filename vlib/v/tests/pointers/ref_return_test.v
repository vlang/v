struct Aa {
	a int
}

fn f() &Aa {
	return &Aa{
		a: 5
	}
}

fn test_ref_return() {
	x := f()
	assert x.a == 5
}
