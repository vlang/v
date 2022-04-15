fn test_cast_in_comptime_if() {
	generic_bool(true)
}

fn generic_bool<T>(val T) {
	$if T is bool {
		println(u8(val))
		assert u8(val) == 1

		println(i8(val))
		assert i8(val) == 1

		println(i16(val))
		assert i16(val) == 1
	} $else {
		assert false
	}
}
