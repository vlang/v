struct Xyz {
mut:
	n int
}

struct Abc {
	a shared Xyz
	i int
}

fn test_shared_struct_auto_struct() {
	e := Abc{}
	shared a := e.a
	lock a {
		a.n = 17
	}
	r := rlock a {
		a.n
	}
	assert r == 17
}

fn test_shared_struct_in_struct() {
	shared y := Xyz{
		n: 7
	}
	z := Abc{
		a: y
	}
	u := Xyz{
		n: 17
	}
	x := Abc{
		a: u
	}
	v := Abc{
		a: Xyz{
			n: 5
		}
		i: 3
	}
	shared f := x.a
	shared g := v.a
	shared h := z.a
	a := rlock f {
		f.n
	}
	b := rlock g {
		g.n
	}
	c := rlock h {
		h.n
	}
	assert a == 17
	assert b == 5
	assert c == 7
	assert v.i == 3
}

struct Efg {
	a shared []f64
	i int
}

fn test_shared_auto_init_array() {
	e := Efg{}
	shared a := unsafe { e.a }
	lock a {
		a << 23.0625
		a << -133.25
		a << 0.125
	}
	r := rlock a {
		a[1]
	}
	assert r == -133.25
}

fn test_shared_array_in_struct() {
	x := Efg{
		a: [1.25, 2.75, 7, 13.0625]
		i: 12
	}
	shared t := unsafe { x.a }
	lock t {
		t[2] = -1.125
	}
	shared tt := unsafe { x.a }
	v := rlock tt {
		tt[3]
	}
	w := rlock tt {
		tt[2]
	}
	assert v == 13.0625
	assert w == -1.125
	assert x.i == 12
}

struct Hjk {
	m shared map[string]f64
	i int
}

fn test_shared_auto_init_map() {
	a := Hjk{}
	shared m := a.m
	lock m {
		m['xcv'] = -31.125
	}
	r := rlock m {
		m['xcv']
	}
	assert r == -31.125
}

fn test_shared_map_in_struct() {
	x := Hjk{
		m: {
			'st': -6.0625
			'xy': 12.125
			'rz': 2.25
		}
		i: 23
	}
	shared k := x.m
	lock k {
		k['yxc'] = -23.5
	}
	shared p := x.m
	a := rlock p {
		p['xy']
	}
	b := rlock p {
		p['yxc']
	}
	assert a == 12.125
	assert b == -23.5
	assert x.i == 23
}

fn test_array_of_shared() {
	mut a := []shared Xyz{cap: 3}
	a0 := Xyz{
		n: 3
	}
	a << a0
	a1 := Xyz{
		n: 7
	}
	a << a1
	a2 := Xyz{
		n: 13
	}
	a << a2
	shared p := a[0]
	shared q := a[2]
	lock q {
		q.n = -17
	}
	shared r := a[2]
	e := rlock p {
		p.n
	}
	f := rlock r {
		r.n
	}
	assert e == 3
	assert f == -17
}
