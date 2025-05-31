fn receive_u64_return_addr(something u64) voidptr {
	return voidptr(something)
}

fn test_u64_to_void_pointer_cast_via_fn_call() {
	a := u64(100)
	b := receive_u64_return_addr(a)
	c := u64(b)
	assert a == c
}
