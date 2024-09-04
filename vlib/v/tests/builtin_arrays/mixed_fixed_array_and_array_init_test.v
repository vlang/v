fn test_mixed_fixed_array_and_array_init1() {
	a := [0.312, 12.0213]!
	b := [0.23, 2131.213]!
	ab := [a, b]
	println(ab)
	assert '${ab}' == '[[0.312, 12.0213], [0.23, 2131.213]]'
}

fn test_mixed_fixed_array_and_array_init2() {
	a := [0.312, 12.0213]!
	b := [0.23, 2131.213]!
	mut ab := [a]
	ab << b
	println(ab)
	assert '${ab}' == '[[0.312, 12.0213], [0.23, 2131.213]]'
}

fn test_mixed_fixed_array_and_array_init3() {
	a := [0.312, 12.0213]!
	b := [0.23, 2131.213]!
	mut ab := [a, b]!
	println(ab)
	assert '${ab}' == '[[0.312, 12.0213], [0.23, 2131.213]]'
}
