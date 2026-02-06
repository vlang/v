fn getsize[P]() u32 {
	return sizeof(P)
}

fn test_sizeof_2() {
	assert getsize[f64]() == 8
	$if new_int ? && x64 {
		assert 8 == getsize[int]()
	} $else {
		assert 4 == getsize[int]()
	}
}
