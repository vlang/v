const sizeof_factor = 2

fn getsize[P]() u32 {
	return sizeof(P)
}

fn accept_usize(value usize) usize {
	return value
}

fn test_sizeof_2() {
	assert getsize[f64]() == 8
	$if new_int ?&& x64 {
		assert 8 == getsize[int]()
	} $else {
		assert 4 == getsize[int]()
	}
}

fn test_sizeof_arithmetic_with_untyped_const() {
	assert accept_usize(sizeof_factor * sizeof(voidptr)) == 2 * sizeof(voidptr)
}
