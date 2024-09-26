module binary

// Little Endian Tests

fn test_little_endian_u16_fixed() {
	assert little_endian_u16_fixed([u8(0), 0]!) == u16(0)
	assert little_endian_u16_fixed([u8(5), 4]!) == u16(0x0405)
	assert little_endian_u16_fixed([u8(0x35), 0x57]!) == u16(0x5735)
	assert little_endian_u16_fixed([u8(0x35), 0x57]!) != u16(0x3557)
}

fn test_little_endian_put_u16_fixed() {
	mut buf := [2]u8{}
	little_endian_put_u16_fixed(mut buf, 0x8725)
	assert buf == [u8(0x25), 0x87]!
	little_endian_put_u16_fixed(mut buf, 0)
	assert buf == [u8(0), 0]!
	little_endian_put_u16_fixed(mut buf, 0xfdff)
	assert buf == [u8(0xff), 0xfd]!
}

fn test_little_endian_u32_fixed() {
	assert little_endian_u32_fixed([u8(0), 0, 0, 0]!) == u32(0)
	assert little_endian_u32_fixed([u8(5), 4, 9, 1]!) == u32(0x01090405)
	assert little_endian_u32_fixed([u8(0xf8), 0xa2, 0x9e, 0x21]!) == u32(0x219ea2f8)
	assert little_endian_u32_fixed([u8(0xf8), 0xa2, 0x9e, 0x21]!) != u32(0xf8a29e21)
}

fn test_little_endian_put_u32_fixed() {
	mut buf := [4]u8{}
	little_endian_put_u32_fixed(mut buf, 0x872fea95)
	assert buf == [u8(0x95), 0xea, 0x2f, 0x87]!
	little_endian_put_u32_fixed(mut buf, 0)
	assert buf == [u8(0), 0, 0, 0]!
	little_endian_put_u32_fixed(mut buf, 0xfdf2e68f)
	assert buf == [u8(0x8f), 0xe6, 0xf2, 0xfd]!
}

fn test_little_endian_u64_fixed() {
	assert little_endian_u64_fixed([u8(0), 0, 0, 0, 0, 0, 0, 0]!) == u64(0)
	assert little_endian_u64_fixed([u8(5), 4, 9, 1, 7, 3, 6, 8]!) == u64(0x0806030701090405)
	assert little_endian_u64_fixed([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]!) == u64(0x8f8e9f7f219ea2f8)
	assert little_endian_u64_fixed([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]!) != u64(0xf8a29e217f9f8e8f)
}

fn test_little_endian_put_u64_fixed() {
	mut buf := [8]u8{}
	little_endian_put_u64_fixed(mut buf, 0x872fea95fdf2e68f)
	assert buf == [u8(0x8f), 0xe6, 0xf2, 0xfd, 0x95, 0xea, 0x2f, 0x87]!
	little_endian_put_u64_fixed(mut buf, 0)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0]!
	little_endian_put_u64_fixed(mut buf, 0xfdf2e68f8e9f7f21)
	assert buf == [u8(0x21), 0x7f, 0x9f, 0x8e, 0x8f, 0xe6, 0xf2, 0xfd]!
}
