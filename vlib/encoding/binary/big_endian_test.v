module binary

// Big Endian Tests
fn test_big_endian_u16() {
	assert big_endian_u16([u8(0), 1]) == u16(1)
	assert big_endian_u16([u8(5), 4]) == u16(0x0504)
	assert big_endian_u16([u8(0x35), 0x57]) == u16(0x3557)
	assert big_endian_u16([u8(0x35), 0x57]) != u16(0x5735)
}

fn test_big_endian_u16_at() {
	assert big_endian_u16_at([u8(0), 0, 1, 0], 1) == u16(1)
	assert big_endian_u16_at([u8(0), 5, 4, 0], 1) == u16(0x0504)
	assert big_endian_u16_at([u8(0), 0x35, 0x57, 0], 1) == u16(0x3557)
	assert big_endian_u16_at([u8(0), 0x35, 0x57, 0], 1) != u16(0x5735)
}

fn test_big_endian_u16_end() {
	assert big_endian_u16_end([u8(0), 0, 0, 1]) == u16(1)
	assert big_endian_u16_end([u8(0), 0, 5, 4]) == u16(0x0504)
	assert big_endian_u16_end([u8(0), 0, 0x35, 0x57]) == u16(0x3557)
	assert big_endian_u16_end([u8(0), 0, 0x35, 0x57]) != u16(0x5735)
}

fn test_big_endian_put_u16() {
	mut buf := []u8{len: 2}
	big_endian_put_u16(mut buf, 0x8725)
	assert buf == [u8(0x87), 0x25]
	big_endian_put_u16(mut buf, 0)
	assert buf == [u8(0), 0]
	big_endian_put_u16(mut buf, 0xfdff)
	assert buf == [u8(0xfd), 0xff]
}

fn test_big_endian_put_u16_at() {
	mut buf := []u8{len: 4}
	big_endian_put_u16_at(mut buf, 0x8725, 1)
	assert buf == [u8(0), 0x87, 0x25, 0]

	buf = []u8{len: 4}
	big_endian_put_u16_at(mut buf, 1, 1)
	assert buf == [u8(0), 0, 1, 0]

	buf = []u8{len: 4}
	big_endian_put_u16_at(mut buf, 0xfdff, 1)
	assert buf == [u8(0), 0xfd, 0xff, 0]
}

fn test_big_endian_get_u16() {
	assert big_endian_get_u16(u16(1)) == [u8(0), 1]
	assert big_endian_get_u16(u16(0x0504)) == [u8(5), 4]
	assert big_endian_get_u16(u16(0x3557)) == [u8(0x35), 0x57]
	assert big_endian_get_u16(u16(0x5735)) != [u8(0x35), 0x57]
}

fn test_big_endian_u32() {
	assert big_endian_u32([u8(0), 0, 0, 1]) == u32(1)
	assert big_endian_u32([u8(5), 4, 9, 1]) == u32(0x05040901)
	assert big_endian_u32([u8(0xf8), 0xa2, 0x9e, 0x21]) == u32(0xf8a29e21)
	assert big_endian_u32([u8(0xf8), 0xa2, 0x9e, 0x21]) != u32(0x2192a2f8)
}

fn test_big_endian_u32_at() {
	assert big_endian_u32_at([u8(0), 0, 0, 0, 1, 0, 0, 0], 1) == u32(1)
	assert big_endian_u32_at([u8(0), 5, 4, 9, 1, 0, 0, 0], 1) == u32(0x05040901)
	assert big_endian_u32_at([u8(0), 0xf8, 0xa2, 0x9e, 0x21, 0, 0, 0], 1) == u32(0xf8a29e21)
	assert big_endian_u32_at([u8(0), 0xf8, 0xa2, 0x9e, 0x21, 0, 0, 0], 1) != u32(0x2192a2f8)
}

fn test_big_endian_u32_end() {
	assert big_endian_u32_end([u8(0), 0, 0, 0, 0, 0, 1]) == u32(1)
	assert big_endian_u32_end([u8(0), 0, 0, 0, 5, 4, 9, 1]) == u32(0x05040901)
	assert big_endian_u32_end([u8(0), 0, 0, 0, 0xf8, 0xa2, 0x9e, 0x21]) == u32(0xf8a29e21)
	assert big_endian_u32_end([u8(0), 0, 0, 0, 0xf8, 0xa2, 0x9e, 0x21]) != u32(0x2192a2f8)
}

fn test_big_endian_put_u32() {
	mut buf := []u8{len: 4}
	big_endian_put_u32(mut buf, 0x872fea95)
	assert buf == [u8(0x87), 0x2f, 0xea, 0x95]
	big_endian_put_u32(mut buf, 0)
	assert buf == [u8(0), 0, 0, 0]
	big_endian_put_u32(mut buf, 0xfdf2e68f)
	assert buf == [u8(0xfd), 0xf2, 0xe6, 0x8f]
}

fn test_big_endian_put_u32_at() {
	mut buf := []u8{len: 8}
	big_endian_put_u32_at(mut buf, 0x872fea95, 1)
	assert buf == [u8(0), 0x87, 0x2f, 0xea, 0x95, 0, 0, 0]

	buf = []u8{len: 8}
	big_endian_put_u32_at(mut buf, 1, 1)
	assert buf == [u8(0), 0, 0, 0, 1, 0, 0, 0]

	buf = []u8{len: 8}
	big_endian_put_u32_at(mut buf, 0xfdf2e68f, 1)
	assert buf == [u8(0), 0xfd, 0xf2, 0xe6, 0x8f, 0, 0, 0]
}

fn test_big_endian_put_u32_end() {
	mut buf := []u8{len: 8}
	big_endian_put_u32_end(mut buf, 0x872fea95)
	assert buf == [u8(0), 0, 0, 0, 0x87, 0x2f, 0xea, 0x95]

	buf = []u8{len: 8}
	big_endian_put_u32_end(mut buf, 1)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 1]

	buf = []u8{len: 8}
	big_endian_put_u32_end(mut buf, 0xfdf2e68f)
	assert buf == [u8(0), 0, 0, 0, 0xfd, 0xf2, 0xe6, 0x8f]
}

fn test_big_endian_get_u32() {
	assert big_endian_get_u32(u32(1)) == [u8(0), 0, 0, 1]
	assert big_endian_get_u32(u32(0x05040901)) == [u8(5), 4, 9, 1]
	assert big_endian_get_u32(u32(0xf8a29e21)) == [u8(0xf8), 0xa2, 0x9e, 0x21]
	assert big_endian_get_u32(u32(0x2192a2f8)) != [u8(0xf8), 0xa2, 0x9e, 0x21]
}

fn test_big_endian_u64() {
	assert big_endian_u64([u8(0), 0, 0, 0, 0, 0, 0, 1]) == u64(1)
	assert big_endian_u64([u8(5), 4, 9, 1, 7, 3, 6, 8]) == u64(0x0504090107030608)
	assert big_endian_u64([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) == u64(0xf8a29e217f9f8e8f)
	assert big_endian_u64([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) != u64(0x8f8e9f7f219ea2f8)
}

fn test_big_endian_u64_at() {
	assert big_endian_u64_at([u8(0), 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0], 1) == u64(1)
	assert big_endian_u64_at([u8(0), 5, 4, 9, 1, 7, 3, 6, 8, 0, 0, 0, 0, 0, 0, 0], 1) == u64(0x0504090107030608)
	assert big_endian_u64_at([u8(0), 0xf8, 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f, 0, 0, 0, 0,
		0, 0, 0], 1) == u64(0xf8a29e217f9f8e8f)
	assert big_endian_u64_at([u8(0), 0xf8, 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f, 0, 0, 0, 0,
		0, 0, 0], 1) != u64(0x8f8e9f7f219ea2f8)
}

fn test_big_endian_u64_end() {
	assert big_endian_u64_end([u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]) == u64(1)
	assert big_endian_u64_end([u8(0), 0, 0, 0, 0, 0, 0, 0, 5, 4, 9, 1, 7, 3, 6, 8]) == u64(0x0504090107030608)
	assert big_endian_u64_end([u8(0), 0, 0, 0, 0, 0, 0, 0, 0xf8, 0xa2, 0x9e, 0x21, 0x7f, 0x9f,
		0x8e, 0x8f]) == u64(0xf8a29e217f9f8e8f)
	assert big_endian_u64_end([u8(0), 0, 0, 0, 0, 0, 0, 0, 0xf8, 0xa2, 0x9e, 0x21, 0x7f, 0x9f,
		0x8e, 0x8f]) != u64(0x8f8e9f7f219ea2f8)
}

fn test_big_endian_put_u64() {
	mut buf := []u8{len: 8}
	big_endian_put_u64(mut buf, 0x872fea95fdf2e68f)
	assert buf == [u8(0x87), 0x2f, 0xea, 0x95, 0xfd, 0xf2, 0xe6, 0x8f]
	big_endian_put_u64(mut buf, 0)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0]
	big_endian_put_u64(mut buf, 0xfdf2e68f8e9f7f21)
	assert buf == [u8(0xfd), 0xf2, 0xe6, 0x8f, 0x8e, 0x9f, 0x7f, 0x21]
}

fn test_big_endian_put_u64_at() {
	mut buf := []u8{len: 16}
	big_endian_put_u64_at(mut buf, 0x872fea95fdf2e68f, 1)
	assert buf == [u8(0), 0x87, 0x2f, 0xea, 0x95, 0xfd, 0xf2, 0xe6, 0x8f, 0, 0, 0, 0, 0, 0, 0]

	buf = []u8{len: 16}
	big_endian_put_u64_at(mut buf, 1, 1)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0]

	buf = []u8{len: 16}
	big_endian_put_u64_at(mut buf, 0xfdf2e68f8e9f7f21, 1)
	assert buf == [u8(0), 0xfd, 0xf2, 0xe6, 0x8f, 0x8e, 0x9f, 0x7f, 0x21, 0, 0, 0, 0, 0, 0, 0]
}

fn test_big_endian_put_u64_end() {
	mut buf := []u8{len: 16}
	big_endian_put_u64_end(mut buf, 0x872fea95fdf2e68f)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0, 0x87, 0x2f, 0xea, 0x95, 0xfd, 0xf2, 0xe6, 0x8f]

	buf = []u8{len: 16}
	big_endian_put_u64_end(mut buf, 1)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]

	buf = []u8{len: 16}
	big_endian_put_u64_end(mut buf, 0xfdf2e68f8e9f7f21)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0, 0xfd, 0xf2, 0xe6, 0x8f, 0x8e, 0x9f, 0x7f, 0x21]
}

fn test_big_endian_get_u64() {
	assert big_endian_get_u64(u64(1)) == [u8(0), 0, 0, 0, 0, 0, 0, 1]
	assert big_endian_get_u64(u64(0x0504090107030608)) == [u8(5), 4, 9, 1, 7, 3, 6, 8]
	assert big_endian_get_u64(u64(0xf8a29e217f9f8e8f)) == [u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f,
		0x8e, 0x8f]
	assert big_endian_get_u64(u64(0x8f8e9f7f219ea2f8)) != [u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f,
		0x8e, 0x8f]
}
