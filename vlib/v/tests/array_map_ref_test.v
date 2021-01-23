fn mod_map(shared m map[string]f64) {
	lock m {
		m['y'] = 6.5
		m['op'] = -13.0625
	}
}

fn mod_array(shared a []f64) {
	lock a {
		a[5] = -13.5
		a[7] = 167.125
	}
}

fn test_array_map_ref() {
	// test initialization
	mut m := map[string]int{}
	mut m_ref := &map[string]f64{}
	mut a := []int{len: 10}
	mut a_ref := &[]f64{cap: 12, len: 2}
	shared m_shared := &map[string]f64{}
	shared a_shared := &[]f64{cap: 12, len: 9}
	// test usage
	m['a'] = 3
	unsafe {
		m_ref['b'] = 12.25
	}
	a << 67
	a << 45
	assert a.len == 12
	a_ref << 73
	a_ref << 12
	a_ref << 8
	unsafe {
		a_ref[1] = 17
	}
	assert a_ref.len == 5
	t1 := go mod_map(shared m_shared)
	t2 := go mod_array(shared a_shared)
	lock m_shared, a_shared {
		a_shared[4] = -12.25
		m_shared['tz'] = 73.75
	}
	t1.wait()
	t2.wait()
	rlock m_shared {
		assert m_shared['y'] == 6.5
		assert m_shared['op'] == -13.0625
		assert m_shared['tz'] == 73.75
	}
	rlock a_shared {
		assert a_shared[4] == -12.25
		assert a_shared[5] == -13.5
		assert a_shared[7] == 167.125
	}
}
