fn counts[T](variables []T) map[T]int {
	mut tally := map[T]int{}
	for var in variables {
		tally[var]++
	}
	return tally
}

fn str_fn() {
	a2 := ['1', '2', '3', '4', '5', '3', '1']
	ret := counts(a2)
	println(ret)
	assert ret.len == 5
}

fn int_fn() {
	a1 := [1, 2, 3, 4, 5, 2, 3]
	ret := counts(a1)
	println(ret)
	assert ret.len == 5
}

fn float_fn() {
	a3 := [0.1, 0.02, 0.3, 4.0, 0.3]
	ret := counts(a3)
	println(ret)
	assert ret.len == 4
}

fn test_generic_map_with_generic_type_key() {
	str_fn()
	float_fn()
	int_fn()
}
