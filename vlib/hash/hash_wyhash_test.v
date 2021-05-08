module hash

struct WyHashTest {
	s        string
	seed     u64
	expected u64
}

fn test_wyhash() {
	tests := [WyHashTest{'', 0, 0x0}, WyHashTest{'v', 1, 0xc72a8f8bdfdd82},
		WyHashTest{'is', 2, 0xa1099c1c58fc13e}, WyHashTest{'the best', 3, 0x1b1215ef0b0b94c},
		WyHashTest{'abcdefghijklmnopqrstuvwxyz', 4, 0x6db0e773d1503fac},
		WyHashTest{'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789', 5, 0xe062dfda99413626},
	]
	for test in tests {
		got := wyhash64(test.s.str, u64(test.s.len), test.seed)
		// println(' #      GOT: $got | $got.hex()')
		// println(' # EXPECTED: $test.expected | $test.expected.hex()')
		assert got == test.expected
	}

	s := '/v/vmaster/vlib/v/fmt/tests/maps_of_fns_with_string_keys_keep.vv'
	x := sum64_string(s, 5).hex_full()
	println(x)
}
