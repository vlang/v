type U24 = [3]u8

fn from_u32(val u32) U24 {
	mut v := U24([3]u8{})
	_ = v[2]
	v[0] = u8((val >> 16) & 0xFF)
	v[1] = u8((val >> 8) & 0xFF)
	v[2] = u8(val & 0xFF)
	return v
}

fn to_u32(v U24) u32 {
	return u32(v[2]) | u32(v[1]) << u8(8) | u32(v[0]) << 16
}

fn test_alias_fixed_array_return() {
	a := u32(1)
	v := from_u32(a)
	assert v == U24([u8(0), 0, 1]!)
	assert '${v}' == 'U24([0, 0, 1])'
	dump(v)
}
