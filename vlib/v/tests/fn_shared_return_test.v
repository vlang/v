struct St {
mut:
	x f64
}

fn f() shared St {
	shared x := St{ x: 3.25 }
	return x
}

fn g(good bool) ?shared St {
	if !good {
		return error('no shared St created')
	}
	shared x := St{ x: 12.75 }
	return x
}

fn test_shared_fn_return() {
	shared x := f()
	val := rlock x { x.x }
	assert val == 3.25
}

fn shared_opt_propagate(good bool) ?f64 {
	shared x := g(good) ?
	ret := rlock x { x.x }
	return ret
}

fn test_shared_opt_propagate() {
	x := shared_opt_propagate(true) or { 1.25 }
	y := shared_opt_propagate(false) or { 2.125 }
	assert x == 12.75
	assert y == 2.125
}

fn test_shared_opt_good() {
	shared yy := g(true) or {
		shared my_default := St{ x: 37.5 }
	    my_default
	}
	val := rlock yy { yy.x }
	assert val == 12.75
}

fn test_shared_opt_bad() {
	shared yy := g(false) or {
		shared my_default := St{ x: 37.5 }
	    my_default
	}
	val := rlock yy { yy.x }
	assert val == 37.5
}
