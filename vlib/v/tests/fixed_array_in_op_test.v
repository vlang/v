fn test_fixed_array_in_op() {
	assert 1 in [1, 2]!
	assert `a` in [`a`, `b`]!
	assert 'a' in ['a', 'b']!

	ch := `"`
	assert ch in [`"`, `'`]!
}
