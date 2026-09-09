@[comptime]
fn pack_color(r u8, g u8, b u8, a u8) u32 {
	return (u32(r) << 24) | (u32(g) << 16) | (u32(b) << 8) | u32(a)
}

enum Colors as u32 {
	red   = pack_color(255, 0, 0, 255)
	green = pack_color(0, 255, 0, 255)
	blue  = pack_color(0, 0, 255, 255)
}

enum CastedColors {
	red   = int(pack_color(1, 2, 3, 4))
	green = int(pack_color(5, 6, 7, 8))
	blue  = int(pack_color(9, 10, 11, 12))
}

fn test_enum_values_from_comptime_function_calls() {
	assert u32(Colors.red) == u32(0xff0000ff)
	assert u32(Colors.green) == u32(0x00ff00ff)
	assert u32(Colors.blue) == u32(0x0000ffff)
}

fn test_enum_values_from_casted_comptime_function_calls() {
	assert int(CastedColors.red) == 0x01020304
	assert int(CastedColors.green) == 0x05060708
	assert int(CastedColors.blue) == 0x090a0b0c
}
