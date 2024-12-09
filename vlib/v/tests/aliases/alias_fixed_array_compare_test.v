type Arr = [4]u8

fn (a Arr) u8_array_fixed() [4]u8 {
	return a
}

fn test_main() {
	a := Arr{}
	b := Arr{}
	assert a.u8_array_fixed() == [u8(0), 0, 0, 0]!
	assert b.u8_array_fixed() == [u8(0), 0, 0, 0]!
	assert a == b

	a_ := a.u8_array_fixed()
	b_ := b.u8_array_fixed()
	assert a_ == b_

	assert a.u8_array_fixed() == b.u8_array_fixed()
}
