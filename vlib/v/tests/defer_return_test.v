struct Qwe {
mut:
	n int
}

fn mut_x(mut x Qwe) &Qwe {
	n := x.n
	// defer statement should not have run, yet
	assert n == 10
	x.n += 5
	return unsafe { &x }
}

fn deferer() &Qwe {
	mut s := &Qwe{
		n: 10
	}
	defer {
		// this should be done after the call to `mut_x()`
		s.n += 2
	}
	return mut_x(mut s)
}

fn test_defer_in_return() {
	q := deferer()
	assert q.n == 12
}
