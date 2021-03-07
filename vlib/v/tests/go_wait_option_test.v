fn f(n int) ?f64 {
	if n < 0 {
		return error('negative number')
	}
	return n + f64(n) / 2
}

fn g(n int) ? {
	if n % 2 == 0 {
		return error('even number')
	} else {
		return
	}
}

fn test_opt_val_wait() {
	h1 := go f(-1)
	h2 := go f(3)
	r1 := h1.wait() or { 17.0 }
	r2 := h2.wait() or { 23.0 }
	assert r1 == 17.0
	assert r2 == 4.5
}

fn test_opt_void_wait() {
	h1 := go g(2)
	h2 := go g(3)
	mut x := 0
	mut y := 0
	h1.wait() or { x = 1 }
	h2.wait() or { y = 1 }
	assert x == 1
	assert y == 0
}

fn propagate(n int, m int) ?f64 {
	h1 := go f(n)
	h2 := go g(m)
	r := h1.wait() ?
	h2.wait() ?
	return r
}

fn test_propagate() {
	x := propagate(5, 3) or { 27.0 }
	y := propagate(-3, 3) or { 29.0 }
	z := propagate(5, 2) or { 31.0 }
	assert x == 7.5
	assert y == 29.0
	assert z == 31.0
}

fn test_array_void_interate() {
	mut r := []thread ?{}
	for i in 0 .. 3 {
		r << go g(i)
	}
	mut res := []int{len: 3, init: 17}
	for i, t in r {
		t.wait() or { res[i] = i }
	}
	assert res[0] == 0
	assert res[1] == 17
	assert res[2] == 2
}

fn test_array_val_interate() {
	mut r := []thread ?f64{}
	for i in -1 .. 2 {
		r << go f(i)
	}
	mut res := []f64{len: 3}
	for i, t in r {
		res[i] = t.wait() or { 17.0 }
	}
	assert res[0] == 17.0
	assert res[1] == 0.0
	assert res[2] == 1.5
}
