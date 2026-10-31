module macho

fn test_too_small() {
	if _ := new_file([]u8{len: 10}) {
		assert false
	} else {
		assert err.msg().contains('too small')
	}
}

fn test_invalid_magic() {
	mut data := []u8{len: 32}
	data[0] = 0xDE
	data[1] = 0xAD
	if _ := new_file(data) {
		assert false
	} else {
		assert err.msg().contains('not a 64-bit Mach-O')
	}
}
