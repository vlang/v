module texture

fn test_ktx2_header() {
	mut bytes := []u8{len: 80}
	sig := [u8(0xAB), 0x4B, 0x54, 0x58, 0x20, 0x32, 0x30, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A]
	for i, b in sig {
		bytes[i] = b
	}
	bytes[12] = 0x59
	bytes[20] = 64
	bytes[24] = 32
	h := parse_ktx2(bytes) or { panic('parse failed') }
	assert h.pixel_width == 64 && h.pixel_height == 32
	assert h.vk_format == 0x59
	assert format_from_vk(h.vk_format) == .dxt1
	assert format_from_vk(9999) == .unknown
}

fn test_bad_ktx2() {
	bytes := []u8{len: 80}
	parse_ktx2(bytes) or {
		assert err.msg() == 'texture: bad KTX2 signature'
	}
	parse_ktx2([]u8{len: 4}) or {
		assert err.msg() == 'texture: KTX2 file too short'
	}
}

fn test_dxt1_block() {
	block := [u8(0x00), 0xF8, 0x1F, 0x00, 0x24, 0x00, 0x00, 0x00]
	pixels := decode_dxt1(block)
	assert pixels.len == 16
	assert pixels[0].r > 200
	assert pixels[1].b > 200
	assert pixels[2].r != pixels[0].r || pixels[2].b != pixels[0].b
	w, h := block_dimensions(8, 9)
	assert w == 2 && h == 3
}
