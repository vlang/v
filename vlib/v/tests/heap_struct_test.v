[heap]
struct Abc {
mut:
	n int
}

struct St {
	Abc
}

struct Qwe {
mut:
	f f64
	a Abc
}

fn pass_abc(q &Abc) &Abc {
	return q
}

fn pass_st(q &St) &St {
	return q
}

fn pass_qwe(q &Qwe) &Qwe {
	return q
}

fn get_ref_structs() (&Abc, &St, &Qwe) {
	a := Abc{ n: 3 }
	b := St{
		Abc{ n: 7 }
	}
	x := Qwe{
		f: 12.25
		a: Abc{ n: 23 }
	}
	aa := pass_abc(&a)
	bb := pass_st(&b)
	xx := pass_qwe(&x)
	return aa, bb, xx
}

fn owerwrite_stack() f64 {
	a := 12.5
	b := 3.5
	c := a + b
	return c
}

fn test_ref_struct() {
	u, v, w := get_ref_structs()
	d := owerwrite_stack()
	assert u.n == 3
	assert v.n == 7
	assert w.a.n == 23
}

