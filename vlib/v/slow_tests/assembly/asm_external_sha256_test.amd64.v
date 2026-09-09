// vtest build: amd64 && !msvc && !tinyc

#flag @VMODROOT/vlib/v/slow_tests/assembly/util/v_sha256_block.o

fn C.v_sha256_block(&u32, &u8)

fn test_external_sha256_block() {
	$if amd64 {
		mut state := [
			u32(0x6a09e667),
			u32(0xbb67ae85),
			u32(0x3c6ef372),
			u32(0xa54ff53a),
			u32(0x510e527f),
			u32(0x9b05688c),
			u32(0x1f83d9ab),
			u32(0x5be0cd19),
		]
		mut block := [64]u8{}
		block[0] = `a`
		block[1] = `b`
		block[2] = `c`
		block[3] = 0x80
		block[63] = 24

		unsafe {
			C.v_sha256_block(&state[0], &block[0])
		}

		expected := [
			u32(0xba7816bf),
			u32(0x8f01cfea),
			u32(0x414140de),
			u32(0x5dae2223),
			u32(0xb00361a3),
			u32(0x96177a9c),
			u32(0xb410ff61),
			u32(0xf20015ad),
		]
		assert state == expected
	}
}
