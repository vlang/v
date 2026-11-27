fn test_reference_array_init() {
	mut b := &[5, 6, 7]
	assert '${b}' == '&[5, 6, 7]'
	mut b_fixed := &[5, 6, 7]!
	assert '${b_fixed}' == '&[5, 6, 7]'
	// b borrows a's array descriptor, so a must outlive the last use of b.
	mut a := [1, 2, 3]
	{
		b = &a
		assert '${b}' == '&[1, 2, 3]'
		b_fixed = &[1, 2, 3]!
	}
	println(b)
	assert '${b}' == '&[1, 2, 3]'
	assert '${b_fixed}' == '&[1, 2, 3]'
}
