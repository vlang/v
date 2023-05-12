struct AA {
mut:
	a int
	b int
	c int
	d int
}

/* fn test() {
	a := AA{}
} */

/* fn aa() (i64, AA) {
	a := 10
	return a, AA{}
} */

/* fn aa() AA {
	return AA{}
}

fn bb() int {
	return 2
} */


/* fn aa() AA {
	return AA{}
}

fn bb() AA {
	return aa()
}

fn cc() AA {
	return bb()
}

fn dd() {
	a := cc()
} */

/* fn aa() AA {
	return AA{}
}

fn bb() {
	a := aa()
} */

/* fn bb() {
	aa()
} */

/* fn cc() int {
	a := bb()
	return a
}

fn dd() {
	a := cc()
} */

/* struct AA {
	a int
	b int
	c i64
	d int
	e int
	f u64
	g int
	h int
} */

/* fn aa() {
	a := AA{}
	b := &a.c
	c := *b
} */

/* fn cond(a bool) (int, i64) {
	return if a {
		10, 50
	} else {
		50, 22
	}
} */

/* fn test() {
	mut a := AA{}
	// a.a = 10
	// a.b = 20
	// b := &a.b
	// c := *b
	b := a
	c := b.b
} */

// fn main() {
// 	a := [20, 25, 27]!
// }

fn ret() int {
	mut c := 0

	for a := 0 ; a < 10 ; a++ {
		c += a
	}

	return c
}