@[comptime]
fn pack_color(r u8, g u8, b u8, a u8) u32 {
	return (u32(r) << 24) | (u32(g) << 16) | (u32(b) << 8) | u32(a)
}

enum Colors as u32 {
	red   = pack_color(255, 0, 0, 255)
	green = pack_color(0, 255, 0, 255)
	blue  = pack_color(0, 0, 255, 255)
}

fn test_enum_values_from_comptime_function_calls() {
	assert u32(Colors.red) == u32(0xff0000ff)
	assert u32(Colors.green) == u32(0x00ff00ff)
	assert u32(Colors.blue) == u32(0x0000ffff)
}
