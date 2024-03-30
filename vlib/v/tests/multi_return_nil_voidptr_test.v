fn ret_int_ptr(arg int) !(int, voidptr) {
	if arg < 0 {
		return error('argument is smaller then zero')
	}
	return 1, [1, 2, 3].data
}

fn test_main() {
	mut val1 := 0
	mut val2 := unsafe { nil }
	val1, val2 = ret_int_ptr(-1) or {
		println(err)
		val1, val2
	}
	assert val1 == 0
	assert val2 == unsafe { nil }
}
