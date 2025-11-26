struct Abc {}

fn bundle(x []u8) !([12]u8, Abc) {
	x12 := [12]u8{}
	return x12, Abc{}
}

fn test_main() {
	a, _ := bundle([])!
	assert a.len == 12
}
