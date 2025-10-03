module chacha20poly1305

// ChaCha20-Poly1305-PSIV test

fn test_psiv_encryption() ! {
}

// The test was adapted from Rust reference implementation of ChaCha20-Poly1305-PSIV
fn test_chacha20_core() ! {
	// null input
	s := [16]u32{}
	x0 := chacha20_core(s)
	assert x0 == s

	// u32 input
	u32input := [u32(0x61707865), 0x3320646e, 0x79622d32, 0x6b206574, 0x03020100, 0x07060504,
		0x0b0a0908, 0x0f0e0d0c, 0x13121110, 0x17161514, 0x1b1a1918, 0x1f1e1d1c, 0x00000001,
		0x09000000, 0x4a000000, 0x00000000]!
	// expected output
	exp_x1 := [u32(0xe4e7f110), 0x15593bd1, 0x1fdd0f50, 0xc47120a3, 0xc7f4d1c7, 0x0368c033,
		0x9aaa2204, 0x4e6cd4c3, 0x466482d2, 0x09aa9f07, 0x05d7c214, 0xa2028bd9, 0xd19c12b5,
		0xb94e16de, 0xe883d0cb, 0x4e3c50a2]!
	x1 := chacha20_core(u32input)
	assert x1 == exp_x1
}
