fn f(n int) ?f64 {
	if n < 0 {
		return error('negative')
	}
	return 1.5 * f64(n)
}

fn test_fn_return() {
	mut res := []f64{cap: 2}
	for m in [-3, 5] {
		if x := f(m) {
			res << x
		} else {
			res << 31.0
		}
	}
	assert res == [31.0, 7.5]
}

fn test_fn_return_empty() {
	if _ := f(-3) {
		assert false
	} else {
		assert true
	}
}

fn test_map_get() {
	mut m := map{
		'xy': 5
		'zu': 7
	}
	mut res := []int{cap: 2}
	for k in ['jk', 'zu'] {
		if x := m[k] {
			res << x
		} else {
			res << -17
		}
	}
	assert res == [-17, 7]
}

fn test_map_get_empty() {
	mut m := map{
		'xy': 5
		'zu': 7
	}
	if _ := m['jk'] {
		assert false
	} else {
		assert true
	}
}

fn test_array_get() {
	mut a := [12.5, 6.5, -17.25]
	mut res := []f64{cap: 2}
	for i in [1, 4] {
		if x := a[i] {
			res << x
		} else {
			res << -23.0
		}
	}
	assert res == [6.5, -23.0]
}

fn test_array_get_empty() {
	mut a := [12.5, 6.5, -17.25]
	if _ := a[7] {
		assert false
	} else {
		assert true
	}
}

fn test_chan_pop() {
	mut res := []f64{cap: 3}
	ch := chan f64{cap: 10}
	ch <- 6.75
	ch <- -3.25
	ch.close()
	for _ in 0 .. 3 {
		if x := <-ch {
			res << x
		} else {
			res << -37.5
		}
	}
	assert res == [6.75, -3.25, -37.5]
}

fn test_chan_pop_empty() {
	ch := chan f64{cap: 10}
	ch <- 6.75
	ch <- -3.25
	ch.close()
	for i in 0 .. 3 {
		if _ := <-ch {
			assert i < 2
		} else {
			assert i == 2
		}
	}
}

struct Thing {
	name string
}

fn test_return_if_guard() {
	ret := option_check('zoo')
	println(ret)
	assert ret == 'zs'
}

fn option_check(name string) string {
	return if thing := find_thing_by_name(name) { thing.name } else { 'safename' }
}

fn find_thing_by_name(name string) ?&Thing {
	return &Thing{
		name: 'zs'
	}
}
