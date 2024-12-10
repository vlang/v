const var = [8]int{init: index}
const var2 = [8]u8{init: u8(index) * 2}

fn test_main() {
	assert var == [int(0), 1, 2, 3, 4, 5, 6, 7]!
	assert var2 == [u8(0), 2, 4, 6, 8, 10, 12, 14]!
}
