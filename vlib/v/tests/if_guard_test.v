fn f(n int) ?f64 {
	if n < 0 {
		return error('negative')
	}
	return 1.5 * f64(n)
}

fn test_fn_return() {
	mut res := []f64{cap:2}
	for m in [-3, 5] {
		if x := f(m) {
			res << x
		} else {
			res << 31.0
		}
	}
	assert res == [31.0, 7.5]
}

fn test_map_get() {
	mut m := {'xy': 5, 'zu': 7}
	mut res := []int{cap:2}
	for k in ['jk', 'zu'] {
		if x := m[k] {
			res << x
		} else {
			res << -17
		}
	}
	assert res == [-17, 7]
}

fn test_array_get() {
	mut a := [12.5, 6.5, -17.25]
	mut res := []f64{cap:2}
	for i in [1, 4] {
		if x := a[i] {
			res << x
		} else {
			res << -23.0
		}
	}
	assert res == [6.5, -23.0]
}

fn test_chan_pop() {
	mut res := []f64{cap:3}
	ch := chan f64{cap: 10}
	ch <- 6.75
	ch <- -3.25
	ch.close()
	for _ in 0 .. 3 {
		if x:= <-ch {
			res << x
		} else {
			res << -37.5
		}
	}
	assert res == [6.75, -3.25, -37.5]
}

