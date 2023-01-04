fn test_fixed_array_in_op() {
	assert 1 in [1, 2]!
	assert `a` in [`a`, `b`]!
	assert 'a' in ['a', 'b']!

	ch := `"`
	assert ch in [`"`, `'`]!

	fixed_arr := [1, 2, 3]!

	b1 := 2 in fixed_arr
	println(b1)
	assert b1

	b2 := 5 !in fixed_arr
	println(b2)
	assert b2
}
