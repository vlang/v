struct Test {
	a bool
	b int
	y string
}

fn test_interpolation_map_to_string() {
	a := map[string]string
	a['1'] = 'one'
	a['2'] = 'two'
	a['3'] = 'three'
	assert '$a' == 'map_string_string{1: one, 2: two, 3: three}'
	b := map[string]int
	b['1'] = 1
	b['2'] = 2
	b['3'] = 3
	assert '$b' == 'map_string_int{1: 1, 2: 2, 3: 3}'
	c := map[string]bool
	c['1'] = true
	c['2'] = false
	assert '$c' == 'map_string_bool{1: true, 2: false}'
	d := map[string]Test
	d['1'] = Test{true, 0, 'abc'}
	d['2'] = Test{true, 1, 'def'}
	d['3'] = Test{false, 2, 'ghi'}
	assert '$d'.replace('\n', '').replace('\'', '"') == 'map_string_Test{1: Test {    a: true    b: 0    y: "abc"}, 2: Test {    a: true    b: 1    y: "def"}, 3: Test {    a: false    b: 2    y: "ghi"}}'
}
