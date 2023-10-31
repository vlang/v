import datatypes

struct Foo {
	bar string
}

fn test_map_reference_value() {
	m1 := map[string]&Foo{}
	if e := m1['bar'] {
		println(e.bar)
	}
	println(m1)

	mut m2 := map[string]&Foo{}
	m2['bar'] = &Foo{}
	println(m2)

	assert true
}

fn test_map_reference_value2() {
	mut m := map[string]&datatypes.Queue[i64]{}
	println('${m}')
	assert '${m}' == '{}'
}
