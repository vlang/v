type Bytes32 = [32]u8

pub fn a(x1 Bytes32, y1 Bytes32, z1 Bytes32, x2 Bytes32, y2 Bytes32, z2 Bytes32) (Bytes32, Bytes32, Bytes32) {
	return Bytes32{}, Bytes32{}, Bytes32{}
}

pub fn b(x Bytes32, y Bytes32, z Bytes32) (Bytes32, Bytes32) {
	return Bytes32{}, Bytes32{}
}

fn test_main() {
	x1 := Bytes32{}
	y1 := Bytes32{}
	z1 := Bytes32{}
	x2 := Bytes32{}
	y2 := Bytes32{}
	z2 := Bytes32{}

	x3, y3, z3 := a(x1, y1, z1, x2, y2, z2)

	xx, yy := b(x3, y3, z3)

	assert xx == yy
}
