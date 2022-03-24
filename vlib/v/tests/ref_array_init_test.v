fn test_reference_array_init() {
	mut b := &[5, 6, 7]
	{
		mut a := [1, 2, 3]
		// TODO: this should probably produce a notice at least,
		// (without unsafe{&a}), since it takes the address of something
		// on the stack, that will very soon be out of scope, even
		// though it is still in the same function:
		b = &a
	}
	println(b)
	assert '$b' == '&[1, 2, 3]'
}
