struct Abc {
mut:
	vptr    voidptr
	vptrptr &voidptr = 0
	bptr    &u8      = 0
	cptr    &char    = c'abcdef'
}

fn test_structs_can_be_printed() {
	a := Abc{}
	println(a)
	assert true
}
