struct Test {
	a bool
	b int
	y string
}

fn test_interpolation_map_to_string() {
	mut a := map[string]string
	a['1'] = 'one'
	a['2'] = 'two'
	a['3'] = 'three'
	assert '$a' == "{'1': 'one', '2': 'two', '3': 'three'}"

	mut b := map[string]int
	b['1'] = 1
	b['2'] = 2
	b['3'] = 3
	assert '$b' == "{'1': 1, '2': 2, '3': 3}"

	mut c := map[string]bool
	c['1'] = true
	c['2'] = false
	assert '$c' == "{'1': true, '2': false}"

	d := {'f1': 1.1, 'f2': 2.2, 'f3': 3.3, 'f4': 4.4}
	println('d: $d')
	assert '$d' == "{'f1': 1.1, 'f2': 2.2, 'f3': 3.3, 'f4': 4.4}"

	mut e := map[string]Test
	e['1'] = Test{true, 0, 'abc'}
	e['2'] = Test{true, 1, 'def'}
	e['3'] = Test{false, 2, 'ghi'}
	s := '$e'
	assert s.contains("{'1': Test{")
	assert s.contains('a: true')
	assert s.contains("y: 'abc'")
	assert s.contains("}, '2': Test{")
	assert s.contains("y: 'def'")

	f := {'hello': [1,2,3]!}
	assert '$f' == "{'hello': [1, 2, 3]}"
}
