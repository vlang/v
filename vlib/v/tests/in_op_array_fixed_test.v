fn test_main() {
	arr := [u8(1), 2, 3, 4]!
	if arr != [4]u8{} || arr != [4]u8{init: 255} {
		println('success')
	}
	if arr !in [[4]u8{}, [4]u8{init: 255}] {
		println('success')
	}
}
