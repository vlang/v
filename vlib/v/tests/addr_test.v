struct AA {
mut:
	a int
}

fn test_addr() {
	mut a := 4
	b := __addr(a)
	a = 3
	assert *b == 3

	mut c := [3, 4, 5]
	d := __addr(c[1])
	c[1] = 3
	assert *d == 3

	mut e := AA{
		a: 4
	}
	f := __addr(e.a)
	e.a = 3
	assert *f == 3
}
