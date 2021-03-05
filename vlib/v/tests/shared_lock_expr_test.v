struct St {
mut:
	i int
}

fn test_lock_expr() {
	shared xx := St{
		i: 173
	}
	shared y := St{
		i: -57
	}
	mut m := 0
	m = lock y {
		y.i
	}
	n := rlock xx {
		xx.i
	}
	assert m == -57
	assert n == 173
}

struct Abc {
mut:
	a f64
}

fn test_multi_objects() {
	shared x := Abc{
		a: 12.5
	}
	shared y := Abc{
		a: -7.5
	}
	shared z := Abc{
		a: 13.125
	}
	a, b, c := rlock z, x, y {
		y.a, z.a, x.a
	}
	assert a == -7.5
	assert b == 13.125
	assert c == 12.5
}

fn (mut st Abc) getvals(mut a Abc, mut b Abc) (f64, f64, f64) {
	return a.a, st.a, b.a
}

fn test_mult_ret_method() {
	shared x := Abc{
		a: 12.5
	}
	shared y := Abc{
		a: -7.5
	}
	shared z := Abc{
		a: 13.125
	}
	a, b, c := lock z, x, y {
		z.getvals(mut x, mut y)
	}
	assert a == 12.5
	assert b == 13.125
	assert c == -7.5
}
