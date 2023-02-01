struct Abc {
mut:
	vptr    voidptr
	vptrptr &voidptr = unsafe { nil }
	bptr    &u8      = unsafe { nil }
	cptr    &char    = c'abcdef'
}

fn test_structs_can_be_printed() {
	a := Abc{}
	println(a)
	assert true
}
