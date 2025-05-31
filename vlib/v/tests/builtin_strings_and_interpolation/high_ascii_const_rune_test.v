const accented = `á`

fn test_high_ascii_const() {
	assert u32(accented) == 225
}
