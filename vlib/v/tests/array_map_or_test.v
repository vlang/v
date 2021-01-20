fn test_array_or() {
	m := [3, 4, 5]
	mut testvar := 17
	el := m[4] or {
		testvar = -43
		999
	}
	good := m[1] or {
		testvar = 11
		0
	}
	assert testvar == -43
	assert el == 999
	assert good == 4
}

fn test_map_or() {
	m := {
		'as': 3
		'qw': 4
		'kl': 5
	}
	mut testvar := -21
	el := m['pp'] or {
		testvar = 7931
		7
	}
	good := m['kl'] or {
		testvar = -45
		999
	}
	assert testvar == 7931
	assert el == 7
	assert good == 5
}


fn get_map_el(key string) ?int {
	m := {'as': 3, 'qw': 4, 'kl': 5}
	r := m[key] ?
	return r
}

fn get_arr_el(i int) ?int {
	m := [3, 4, 5]
	r := m[i] ?
	return r
}

fn test_propagation() {
	mut testvar1 := 12
	mut testvar2 := 78
	e := get_map_el('vv') or {
		testvar1 = -34
		7
	}
	f := get_map_el('as') or {
		testvar1 = 67
		23
	}
	g := get_arr_el(3) or {
		testvar2 = 99
		12
	}
	h := get_arr_el(0) or {
		testvar2 = 177
		int(-67)
	}
	assert testvar1 == -34
	assert testvar2 == 99
	assert e == 7
	assert f == 3
	assert g == 12
	assert h == 3
}

fn get_arr_el_nested(i int) ?int {
	ind := [2, 1, 0, 5]
	m := [3, 4, 5]
	r := m[ind[i]] ?
	return r
}

fn test_nested_array_propagation() {
	g := get_arr_el_nested(3) or {
		12
	}
	assert g == 12
}
