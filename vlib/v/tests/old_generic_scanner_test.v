module main

fn test_main() {
	p := 100
	for i := 0; i < 4; i += 4 {
		// keep following comment:
		// p := unsafe {pixels[i]}
		r := u8(p >> 24)
		assert r == 0
	}
}
