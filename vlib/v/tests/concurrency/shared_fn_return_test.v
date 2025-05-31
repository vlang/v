struct St {
mut:
	x f64
}

fn f() St {
	x := St{
		x: 3.25
	}
	return x
}

fn g(good bool) ?St {
	if !good {
		return none
	}
	x := St{
		x: 12.75
	}
	return x
}

fn r(good bool) !St {
	if !good {
		return error('no St created')
	}
	x := St{
		x: 12.75
	}
	return x
}

fn test_shared_fn_return() {
	shared x := f()
	val := rlock x {
		x.x
	}
	assert val == 3.25
}

fn shared_opt_propagate(good bool) ?f64 {
	shared x := g(good)?
	ret := rlock x {
		x.x
	}
	return ret
}

fn shared_err_propagate(good bool) !f64 {
	shared x := r(good)!
	ret := rlock x {
		x.x
	}
	return ret
}

fn test_shared_opt_propagate() {
	mut x := shared_opt_propagate(true) or { 1.25 }
	mut y := shared_opt_propagate(false) or { 2.125 }
	assert x == 12.75
	assert y == 2.125

	x = shared_err_propagate(true) or { 1.25 }
	y = shared_err_propagate(false) or { 2.125 }
	assert x == 12.75
	assert y == 2.125
}

fn test_shared_opt_good() {
	shared yy := g(true) or {
		St{
			x: 37.5
		}
	}
	val := rlock yy {
		yy.x
	}
	assert val == 12.75
}

fn test_shared_opt_bad() {
	shared yy := g(false) or {
		St{
			x: 37.5
		}
	}
	val := rlock yy {
		yy.x
	}
	assert val == 37.5
}
