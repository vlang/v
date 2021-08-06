[heap]
struct Hwe {
mut:
	n int
}

fn mut_x(mut x Hwe) &Hwe {
	n := x.n
	// defer statement should not have run, yet
	assert n == 10
	x.n += 5
	return &x
}

fn deferer() &Hwe {
	mut s := &Hwe{
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
	// both `mut_x()` and `defer` have been run
	assert q.n == 17
}

struct Qwe {
mut:
	n int
}

fn ret_ref(mut x Qwe) &Qwe {
	return unsafe { x }
}

fn defer_multi_ret(mut a Qwe) (int, f64) {
	defer {
		a.n *= 2
	}
	// the return values should be calculated before `a.n` is doubled
	return 3 * a.n, 2.5 * f64(a.n)
}

fn test_defer_in_multi_return() {
	mut x := Qwe{
		n: 3
	}
	y, z := defer_multi_ret(mut x)
	assert x.n == 6
	assert y == 9
	assert z == 7.5
}

fn option_return_good(mut a Qwe) ?Qwe {
	defer {
		a.n += 7
	}
	a.n += 3
	return a
}

fn test_defer_opt_return() {
	mut x := Qwe{
		n: -2
	}
	y := option_return_good(mut x) or {
		Qwe{
			n: -100
		}
	}
	assert x.n == 8
	assert y.n == 1
}

fn option_return_err(mut a Qwe) ?Qwe {
	defer {
		a.n += 5
	}
	a.n += 2
	return error('Error: ${a.n}')
}

fn test_defer_err_return() {
	mut x := Qwe{
		n: 17
	}
	mut e_msg := ''
	y := option_return_err(mut x) or {
		e_msg = '${err}'
		Qwe{
			n: -119
		}
	}
	assert x.n == 24
	assert y.n == -119
	assert e_msg == 'Error: 19'
}

fn return_opt_call(mut a Qwe) ?Qwe {
	defer {
		a.n += 5
	}
	a.n += 2
	return option_return_good(mut a)
}

fn test_defer_option_call() {
	mut x := Qwe{
		n: -1
	}
	b := return_opt_call(mut x) or { Qwe{} }
	assert x.n == 16
	assert b.n == 4
}
