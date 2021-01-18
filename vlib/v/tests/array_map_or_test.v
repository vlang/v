fn test_array_or() {
	m := [3, 4, 5]
	mut testvar := 17
	el := m[4] or {
		testvar = -43
	}
	good := m[1] or {
		testvar = 11
	}
	assert testvar == -43
	assert el == 0
	assert good == 4
}

fn test_map_or() {
	m := {'as': 3, 'qw': 4, 'kl': 5}
	mut testvar := -21
	el := m['pp'] or {
		testvar = 7931
	}
	good := m['kl'] or {
		testvar = -45
	}
	assert testvar == 7931
	assert el == 0
	assert good == 5
}
