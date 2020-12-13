fn test_cmp_signed_and_u32() {
	// ==
	assert int(1) == u32(1)
	// !=
	assert int(1) != u32(2)
	// >
	assert !(int(1) > u32(1))
	assert int(1) > u32(0)
	// >=
	assert !(int(0) >= u32(1))
	assert int(1) >= u32(1)
	assert int(1) >= u32(0)
	// <
	assert !(int(1) < u32(1))
	assert int(0) < u32(1)
	// <=
	assert int(0) <= u32(1)
	assert int(1) <= u32(1)
	assert !(int(1) <= u32(0))
}

fn test_cmp_u32_and_signed() {
	// ==
	assert u32(1) == int(1)
	// !=
	assert u32(2) != int(1)
	// >
	assert !(u32(1) > int(1))
	assert u32(1) > int(0)
	// >=
	assert u32(1) >= int(0)
	assert u32(1) >= int(1)
	assert !(u32(0) >= int(1))
	// <
	assert !(u32(1) < int(1))
	assert u32(0) < int(1)
	// <=
	assert u32(0) <= int(1)
	assert u32(1) <= int(1)
	assert !(u32(1) <= int(0))
}

fn test_cmp_signed_and_u64() {
	// ==
	assert int(1) == u64(1)
	// !=
	assert int(1) != u64(2)
	// >
	assert !(int(1) > u64(1))
	assert int(1) > u64(0)
	// >=
	assert !(int(0) >= u64(1))
	assert int(1) >= u64(1)
	assert int(1) >= u64(0)
	// <
	assert !(int(1) < u64(1))
	assert int(0) < u64(1)
	// <=
	assert int(0) <= u64(1)
	assert int(1) <= u64(1)
	assert !(int(1) <= u64(0))
}

fn test_cmp_u64_and_signed() {
	// ==
	assert u64(1) == int(1)
	// !=
	assert u64(2) != int(1)
	// >
	assert !(u64(1) > int(1))
	assert u64(1) > int(0)
	// >=
	assert u64(1) >= int(0)
	assert u64(1) >= int(1)
	assert !(u64(0) >= int(1))
	// <
	assert !(u64(1) < int(1))
	assert u64(0) < int(1)
	// <=
	assert u64(0) <= int(1)
	assert u64(1) <= int(1)
	assert !(u64(1) <= int(0))
}
