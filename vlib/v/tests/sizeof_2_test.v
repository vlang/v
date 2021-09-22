fn getsize<P>() u32 {
	return sizeof(P)
}

fn test_sizeof_2() {
	assert getsize<f64>() == 8
	assert 4 == getsize<int>()
}
