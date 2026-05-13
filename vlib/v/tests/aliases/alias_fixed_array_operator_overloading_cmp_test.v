type Addr = [4]u8

fn (a Addr) u32() u32 {
	return (u32(a[0]) << 24) | (u32(a[1]) << 16) | (u32(a[2]) << 8) | u32(a[3])
}

fn (a Addr) < (b Addr) bool {
	return a.u32() < b.u32()
}

fn test_fixed_array_alias_derived_comparison_operators_work() {
	a := Addr([u8(1), 2, 3, 4]!)
	b := Addr([u8(5), 4, 3, 2]!)

	assert a < b
	assert a <= b
	assert b > a
	assert b >= a
	assert a != b
}
