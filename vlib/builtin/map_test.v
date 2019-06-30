struct A {
	m map_int
}

fn (a mut A) set(key string, val int) {
	a.m[key] = val
}

fn test_map() {
	mut m := map[string]int{}
	m['hi'] = 80
	assert m['hi'] == 80

	mut a := A{
		m: new_map(1, sizeof(int))
	}
	a.m['one'] = 1
	a.set('two', 2)
	assert a.m['one'] == 1
	assert a.m['two'] == 2
}
