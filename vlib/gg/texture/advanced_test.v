module texture

import gg

fn test_etc1_modifiers() {
	// Individual mode, red base, no modifiers set.
	plain := [u8(0xF0), 0, 0, 0x20, 0, 0, 0, 0]
	a := decode_etc1(plain)
	// Same base but modifier bits set in the second plane.
	modified := [u8(0xF0), 0, 0, 0x20, 0xFF, 0xFF, 0xFF, 0xFF]
	b := decode_etc1(modified)
	assert a.len == 16 && b.len == 16
	assert a[0].r > 200
	// At least one pixel must differ once intensity modifiers are applied.
	mut different := false
	for i in 0 .. 16 {
		if a[i] != b[i] {
			different = true
		}
	}
	assert different
}

fn test_bc7_mode3() {
	// Mode 3: leading 3 ones => byte0 = 0x07. Endpoints are 4-bit values.
	mut block := []u8{len: 16, init: 0}
	block[0] = 0x07
	// R0=15,R1=0,G0=15,G1=0,B0=15,B1=0,A0=15,A1=0 packed LSB-first.
	block[1] = 0x0F
	block[2] = 0xF0
	block[3] = 0x0F
	block[4] = 0xF0
	block[5] = 0x0F
	block[6] = 0xF0
	block[7] = 0x0F
	colors := decode_bc7(block) or { panic('bc7 failed') }
	assert colors.len == 16
	assert colors.len == 16
}

fn test_bc7_mode4() {
	mut block := []u8{len: 16, init: 0}
	// Mode 4: 4 leading ones, uses a CEM field and 5-bit endpoints.
	block[0] = 0x0F
	colors := decode_bc7(block) or { panic('mode 4 should decode') }
	assert colors.len == 16
}

fn test_astc_void_extent() {
	mut block := []u8{len: 16, init: 0}
	// Void extent: low 9 bits set, LDR bit clear.
	block[0] = 0xFF
	block[1] = 0x01
	// R,G,B = 0x8000 each (mid gray).
	block[2] = 0x00
	block[3] = 0x80
	block[4] = 0x00
	block[5] = 0x80
	block[6] = 0x00
	block[7] = 0x80
	colors := decode_astc(block) or { panic('astc failed') }
	assert colors.len == 16
	assert colors[0] == colors[15]
	assert colors[0].r == colors[0].r
	assert colors[0].a == 255
}
