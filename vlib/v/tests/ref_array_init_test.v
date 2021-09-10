fn test_reference_array_init() {
	mut b := &[5, 6, 7]
	{
		a := [1, 2, 3]
		b = &a
	}
	println(b)
	assert '$b' == '&[1, 2, 3]'
}
