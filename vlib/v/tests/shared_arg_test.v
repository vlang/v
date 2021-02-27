struct St {
mut:
	a int
}

struct Qr {
mut:
	a int
}

fn (mut r Qr) s_mut(mut s St) {
	r.a = 5
	s.a = 7
}

fn (r Qr) s_val(s St) int {
	return r.a * s.a
}

fn m_mut(mut a map[string]f64) {
	a['yxcv'] = -2.25
}

fn m_val(a map[string]f64) f64 {
	x := a['yxcv']
	return x
}

fn a_mut(mut a []int) {
	a[2] = 42
}

fn a_val(a []int) int {
	return a[1]
}

fn test_shared_as_value() {
	shared s := St{ a: 5 }
	shared a := [3, 4, 6, 13, -23]
	shared m := {'qw': 12.75, 'yxcv': -3.125, 'poiu': 88.0625}
	shared r := Qr{ a: 7 }
	rlock s, r {
		u := r.s_val(s)
		assert u == 35
	}
	lock s, r {
		v := r.s_val(s)
		assert v == 35
	}
	rlock m {
		u := m_val(m)
		assert u == -3.125
	}
	lock a {
		u := a_val(a)
		assert u == 4
	}
}

fn test_shared_as_mut() {
	shared s := St{ a: 5 }
	shared a := [3, 4, 6, 13, -23]
	shared m := {'qw': 12.75, 'yxcv': -3.125, 'poiu': 88.0625}
	shared r := Qr{ a: 7 }
	lock s, r {
		r.s_mut(mut s)
		x := r.a * s.a
		assert x == 35
	}
	lock a, m {
		m_mut(mut m)
		a_mut(mut a)
		y := m['yxcv']
		z := a[2]
		assert y == -2.25
		assert z == 42
	}
}
