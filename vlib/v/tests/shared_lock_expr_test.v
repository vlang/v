struct St {
mut:
	i int
}

fn test_lock_expr() {
	shared xx := St{
		i: 173
	}
	shared y := St{
		i: -57
	}
	mut m := 0
	m = lock y {
		y.i
	}
	n := rlock xx {
		xx.i
	}
	assert m == -57
	assert n == 173
}

struct Abc {
mut:
	a f64
}

fn test_multi_objects() {
	shared x := Abc{
		a: 12.5
	}
	shared y := Abc{
		a: -7.5
	}
	shared z := Abc{
		a: 13.125
	}
	a, b, c := rlock z, x, y {
		y.a, z.a, x.a
	}
	assert a == -7.5
	assert b == 13.125
	assert c == 12.5
}

fn (mut st Abc) getvals(mut a Abc, mut b Abc) (f64, f64, f64) {
	return a.a, st.a, b.a
}

fn test_mult_ret_method() {
	shared x := Abc{
		a: 12.5
	}
	shared y := Abc{
		a: -7.5
	}
	shared z := Abc{
		a: 13.125
	}
	a, b, c := lock z, x, y {
		z.getvals(mut x, mut y)
	}
	assert a == 12.5
	assert b == 13.125
	assert c == -7.5
}

fn test_shared_lock_map_index_expr() {
	shared m := map{
		'qwe': 'asd'
	}
	for key in ['fdf', 'qwe'] {
		v := rlock m {
			m[key] or { 'key not found' }
		}
		if key == 'qwe' {
			assert v == 'asd'
		} else {
			assert v == 'key not found'
		}
	}
}

fn test_shared_lock_array_index_expr() {
	shared a := [-12.5, 6.25]
	for i in -2 .. 4 {
		v := rlock a {
			a[i] or { 23.75 }
		}
		if i == 0 {
			assert v == -12.5
		} else if i == 1 {
			assert v == 6.25
		} else {
			assert v == 23.75
		}
	}
}

struct DummySt {
	a int
}

fn test_shared_lock_chan_rec_expr() {
	ch := chan int{cap: 10}
	shared st := DummySt{}
	ch <- 7
	ch <- -13
	ch.close()
	for i in 0 .. 3 {
		v := rlock st {
			<-ch or { -17 }
		}
		if i == 0 {
			assert v == 7
		} else if i == 1 {
			assert v == -13
		} else {
			assert v == -17
		}
	}
}
