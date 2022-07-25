fn multi_voidptr_ret() (voidptr, bool) {
	return unsafe { nil }, true
}

fn multi_byteptr_ret() (&u8, bool) {
	return &u8(0), true
}

fn test_multi_ptrtype_ret() {
	a, b := multi_voidptr_ret()
	assert u64(a) == 0
	assert b == true
	c, d := multi_byteptr_ret()
	assert u64(c) == 0
	assert d == true
}
