module main

fn ret() [2]int {
	return [3, 4]!
}

fn test_main() {
	coords := [ret()]
	assert coords[0] == [3, 4]!
}
