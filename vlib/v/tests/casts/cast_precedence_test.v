fn test_main() {
	assert (u8(1) << 1 | 1) == 3
	assert (match 0 {
		0 { u8(1) << 1 | 1 }
		else { 0 }
	}) == 3
	assert (if true {
		u8(1) << 1 | 1
	} else {
		0
	}) == 3
}
