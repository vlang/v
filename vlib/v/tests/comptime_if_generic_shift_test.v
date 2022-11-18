fn generic<T>(val T) T {
	$if T is u64 {
		println(val << 1)
		return val << 1
	}
	return val
}

fn test_comptime_if_generic_shift() {
	ret := generic(u64(2))
	println(ret)
	assert ret == 4
}
