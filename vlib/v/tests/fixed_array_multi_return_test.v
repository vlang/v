module main

pub fn a(x1 [32]u8, y1 [32]u8, z1 [32]u8, x2 [32]u8, y2 [32]u8, z2 [32]u8) ([32]u8, [32]u8, [32]u8) {
	return [32]u8{}, [32]u8{}, [32]u8{}
}

pub fn b(x [32]u8, y [32]u8, z [32]u8) ([32]u8, [32]u8) {
	return [32]u8{}, [32]u8{}
}

fn test_main() {
	x1 := [32]u8{}
	y1 := [32]u8{}
	z1 := [32]u8{}
	x2 := [32]u8{}
	y2 := [32]u8{}
	z2 := [32]u8{}

	x3, y3, z3 := a(x1, y1, z1, x2, y2, z2)

	xx, yy := b(x3, y3, z3)

	assert xx == yy
}
