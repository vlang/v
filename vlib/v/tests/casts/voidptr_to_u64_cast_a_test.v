fn receive_addr_return_u64(addr voidptr) u64 {
	return u64(addr)
}

fn test_void_pointer_to_u64_cast_via_fn_call() {
	a := u64(10)
	b := voidptr(a)
	c := receive_addr_return_u64(b)
	assert a == c
}
