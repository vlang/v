module texture

import gg

fn test_etc1_block() {
	// Solid block: base colors red in both halves.
	block := [u8(0xF0), 0, 0, 0x20, 0, 0, 0, 0]
	colors := decode_etc1(block)
	assert colors.len == 16
	assert colors[0].r > 180 && colors[0].g < 10 && colors[0].b < 10
	assert colors[15].a == 255
}

fn test_mipmaps() {
	mut src := []u8{len: 4 * 4 * 4}
	for i in 0 .. src.len {
		src[i] = 128
	}
	levels := generate_mipmaps(src, 4, 4)
	assert levels.len == 3
	assert levels[0].len == 64
	assert levels[1].len == 16
	assert levels[2].len == 4
	assert levels[1][0] == 128
	assert levels[2][0] == 128
	_ = gg.Color{}
}
