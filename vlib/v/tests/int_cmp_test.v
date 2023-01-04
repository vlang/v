fn test_cmp_signed() {
	assert i8(3) > i16(-10)
	assert i16(-9) > int(-11)
	assert i64(-12) <= i8(-12)
	assert i64(-43232554) < i8(-126)
}

fn test_cmp_unsigned() {
	assert u8(3) < u16(10)
	assert u16(40000) > u32(200)
	assert u64(18161419857654944321) >= u8(12)
	assert u64(40000) < u16(40001)
}

fn test_cmp_unsigned_signed() {
	assert u8(12) > i8(-12)
	assert i16(-27) < u32(65463356)
	assert u32(8543) > int(-7523)
	assert i64(-89) <= u64(567)
	assert int(-1) != u32(0xffffffff)
	assert !(u64(0xfffffffffffffffe) == i64(-2))
}
