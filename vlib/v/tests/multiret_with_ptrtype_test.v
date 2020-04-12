fn multi_voidptr_ret() (voidptr, bool) {
	return voidptr(0), true
}

fn multi_byteptr_ret() (byteptr, bool) {
	return byteptr(0), true
}

fn test_multi_ptrtype_ret() {
	a, b := multi_voidptr_ret()
	assert a == voidptr(0)
	assert b == true
	c, d := multi_byteptr_ret()
	assert c == byteptr(0)
	assert d == true
}
