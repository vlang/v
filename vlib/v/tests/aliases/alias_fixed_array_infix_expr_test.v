import encoding.binary

pub type Addr = [4]u8

pub fn (a Addr) u32() u32 {
	return binary.big_endian_u32_fixed(a)
}

pub fn (a Addr) == (oth Addr) bool {
	return a.u32() == oth.u32()
}

pub fn (a Addr) < (oth Addr) bool {
	return a.u32() < oth.u32()
}

fn test_alias_fixed_array_infix_expr() {
	addr := Addr([u8(127), 0, 0, 1]!)

	assert addr == Addr([u8(127), 0, 0, 1]!)
	assert Addr([u8(127), 0, 0, 1]!) == addr

	assert addr == [u8(127), 0, 0, 1]!
	assert [u8(127), 0, 0, 1]! == addr

	assert addr < Addr([u8(127), 0, 0, 2]!)
	assert addr < [u8(127), 0, 0, 2]!
}
