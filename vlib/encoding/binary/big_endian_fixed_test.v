module binary

// Big Endian Tests
fn test_big_endian_u16_fixed() {
	assert big_endian_u16_fixed([u8(0), 1]!) == u16(1)
	assert big_endian_u16_fixed([u8(5), 4]!) == u16(0x0504)
	assert big_endian_u16_fixed([u8(0x35), 0x57]!) == u16(0x3557)
	assert big_endian_u16_fixed([u8(0x35), 0x57]!) != u16(0x5735)
}

fn test_big_endian_put_u16_fixed() {
	mut buf := [2]u8{}
	big_endian_put_u16_fixed(mut buf, 0x8725)
	assert buf == [u8(0x87), 0x25]!
	big_endian_put_u16_fixed(mut buf, 0)
	assert buf == [u8(0), 0]!
	big_endian_put_u16_fixed(mut buf, 0xfdff)
	assert buf == [u8(0xfd), 0xff]!
}

fn test_big_endian_u32_fixed() {
	assert big_endian_u32_fixed([u8(0), 0, 0, 1]!) == u32(1)
	assert big_endian_u32_fixed([u8(5), 4, 9, 1]!) == u32(0x05040901)
	assert big_endian_u32_fixed([u8(0xf8), 0xa2, 0x9e, 0x21]!) == u32(0xf8a29e21)
	assert big_endian_u32_fixed([u8(0xf8), 0xa2, 0x9e, 0x21]!) != u32(0x2192a2f8)
}

fn test_big_endian_put_u32_fixed() {
	mut buf := [4]u8{}
	big_endian_put_u32_fixed(mut buf, 0x872fea95)
	assert buf == [u8(0x87), 0x2f, 0xea, 0x95]!
	big_endian_put_u32_fixed(mut buf, 0)
	assert buf == [u8(0), 0, 0, 0]!
	big_endian_put_u32_fixed(mut buf, 0xfdf2e68f)
	assert buf == [u8(0xfd), 0xf2, 0xe6, 0x8f]!
}

fn test_big_endian_u64_fixed() {
	assert big_endian_u64_fixed([u8(0), 0, 0, 0, 0, 0, 0, 1]!) == u64(1)
	assert big_endian_u64_fixed([u8(5), 4, 9, 1, 7, 3, 6, 8]!) == u64(0x0504090107030608)
	assert big_endian_u64_fixed([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]!) == u64(0xf8a29e217f9f8e8f)
	assert big_endian_u64_fixed([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]!) != u64(0x8f8e9f7f219ea2f8)
}

fn test_big_endian_put_u64_fixed() {
	mut buf := [8]u8{}
	big_endian_put_u64_fixed(mut buf, 0x872fea95fdf2e68f)
	assert buf == [u8(0x87), 0x2f, 0xea, 0x95, 0xfd, 0xf2, 0xe6, 0x8f]!
	big_endian_put_u64_fixed(mut buf, 0)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0]!
	big_endian_put_u64_fixed(mut buf, 0xfdf2e68f8e9f7f21)
	assert buf == [u8(0xfd), 0xf2, 0xe6, 0x8f, 0x8e, 0x9f, 0x7f, 0x21]!
}
