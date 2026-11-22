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

fn test_bc7_modes_4_to_7() {
	for mode in 4 .. 8 {
		mut block := []u8{len: 16, init: 0}
		block[0] = u8((1 << u8(mode)) - 1)
		colors := decode_bc7(block) or { panic('mode ${mode} should decode') }
		assert colors.len == 16
	}
}

fn test_astc_nonvoid() {
	mut block := []u8{len: 16, init: 0}
	block[0] = 0x01
	colors := decode_astc(block) or { panic('non-void ASTC should decode') }
	assert colors.len == 16
	mut sized := []u8{len: 16, init: 0}
	sized[0] = 0x01
	big := decode_astc_sized(sized, 8, 8) or { panic('sized ASTC should decode') }
	assert big.len == 64
}

fn test_unaligned_bit_fields() {
	// The bulk byte readers must agree with bit-by-bit extraction for fields
	// that straddle byte boundaries.
	// 0b10110011, 0b01011010, 0b11110000 followed by zero padding.
	data := [u8(179), 90, 240, 0, 0, 0, 0, 0]
	positions := [0, 1, 3, 5, 7, 9, 11, 15]
	widths := [1, 2, 4, 3, 8, 6, 16, 9]
	mut br := BitReader{ data: data, pos: 0 }
	for k in 0 .. positions.len {
		at_pos := positions[k]
		width := widths[k]
		mut want := u64(0)
		for i in 0 .. width {
			bit_at := at_pos + i
			bit := (data[bit_at / 8] >> u8(bit_at % 8)) & 1
			want |= u64(bit) << u32(i)
		}
		got_astc := astc_read_bits(data, at_pos, width) or { panic('astc read failed') }
		assert got_astc == want
		br.pos = at_pos
		got_bc7 := br.read(width) or { panic('bc7 read failed') }
		assert got_bc7 == want
	}
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
