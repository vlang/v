module binary

// Little Endian Tests

fn test_little_endian_u16() {
	assert little_endian_u16([u8(0), 0]) == u16(0)
	assert little_endian_u16([u8(5), 4]) == u16(0x0405)
	assert little_endian_u16([u8(0x35), 0x57]) == u16(0x5735)
	assert little_endian_u16([u8(0x35), 0x57]) != u16(0x3557)
}

fn test_little_endian_u16_at() {
	assert little_endian_u16_at([u8(1), 0, 0, 1], 1) == u16(0)
	assert little_endian_u16_at([u8(5), 4, 9, 1], 1) == u16(0x0904)
	assert little_endian_u16_at([u8(0xf8), 0xa2, 0x9e, 0x21], 1) == u16(0x9ea2)
	assert little_endian_u16_at([u8(0xf8), 0xa2, 0x9e, 0x21], 1) != u16(0xa29e)
}

fn test_little_endian_u16_end() {
	assert little_endian_u16_end([u8(1), 0, 0, 1]) == u16(0x0100)
	assert little_endian_u16_end([u8(5), 4, 9, 1]) == u16(0x0109)
	assert little_endian_u16_end([u8(0xf8), 0xa2, 0x9e, 0x21]) == u16(0x219e)
	assert little_endian_u16_end([u8(0xf8), 0xa2, 0x9e, 0x21]) != u16(0x9e21)
}

fn test_little_endian_put_u16() {
	mut buf := []u8{len: 2}
	little_endian_put_u16(mut buf, 0x8725)
	assert buf == [u8(0x25), 0x87]
	little_endian_put_u16(mut buf, 0)
	assert buf == [u8(0), 0]
	little_endian_put_u16(mut buf, 0xfdff)
	assert buf == [u8(0xff), 0xfd]
}

fn test_little_endian_put_u16_at() {
	mut buf := []u8{len: 4}
	little_endian_put_u16_at(mut buf, 0x8725, 1)
	assert buf == [u8(0), 0x25, 0x87, 0]

	buf = []u8{len: 4}
	little_endian_put_u16_at(mut buf, 1, 1)
	assert buf == [u8(0), 1, 0, 0]

	buf = []u8{len: 4}
	little_endian_put_u16_at(mut buf, 0xfdff, 1)
	assert buf == [u8(0), 0xff, 0xfd, 0]
}

fn test_little_endian_put_u16_end() {
	mut buf := []u8{len: 4}
	little_endian_put_u16_end(mut buf, 0x8725)
	assert buf == [u8(0), 0, 0x25, 0x87]

	buf = []u8{len: 4}
	little_endian_put_u16_end(mut buf, 1)
	assert buf == [u8(0), 0, 1, 0]

	buf = []u8{len: 4}
	little_endian_put_u16_end(mut buf, 0xfdff)
	assert buf == [u8(0), 0, 0xff, 0xfd]
}

fn test_little_endian_u32() {
	assert little_endian_u32([u8(0), 0, 0, 0]) == u32(0)
	assert little_endian_u32([u8(5), 4, 9, 1]) == u32(0x01090405)
	assert little_endian_u32([u8(0xf8), 0xa2, 0x9e, 0x21]) == u32(0x219ea2f8)
	assert little_endian_u32([u8(0xf8), 0xa2, 0x9e, 0x21]) != u32(0xf8a29e21)
}

fn test_little_endian_u32_at() {
	assert little_endian_u32_at([u8(1), 0, 0, 0, 0, 0, 0, 0], 1) == u32(0)
	assert little_endian_u32_at([u8(5), 4, 9, 1, 7, 3, 6, 8], 1) == u32(0x07010904)
	assert little_endian_u32_at([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f],
		1) == u32(0x7f219ea2)
	assert little_endian_u32_at([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f],
		1) != u32(0xa29e217f)
}

fn test_little_endian_u32_end() {
	assert little_endian_u32_end([u8(1), 0, 0, 0, 0, 0, 0, 0]) == u32(0)
	assert little_endian_u32_end([u8(5), 4, 9, 1, 7, 3, 6, 8]) == u32(0x08060307)
	assert little_endian_u32_end([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) == u32(0x8f8e9f7f)
	assert little_endian_u32_end([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) != u32(0x7f9f8e8f)
}

fn test_little_endian_put_u32() {
	mut buf := []u8{len: 4}
	little_endian_put_u32(mut buf, 0x872fea95)
	assert buf == [u8(0x95), 0xea, 0x2f, 0x87]
	little_endian_put_u32(mut buf, 0)
	assert buf == [u8(0), 0, 0, 0]
	little_endian_put_u32(mut buf, 0xfdf2e68f)
	assert buf == [u8(0x8f), 0xe6, 0xf2, 0xfd]
}

fn test_little_endian_put_u32_at() {
	mut buf := []u8{len: 8}
	little_endian_put_u32_at(mut buf, 0x872fea95, 1)
	assert buf == [u8(0), 0x95, 0xea, 0x2f, 0x87, 0, 0, 0]

	buf = []u8{len: 8}
	little_endian_put_u32_at(mut buf, 1, 1)
	assert buf == [u8(0), 1, 0, 0, 0, 0, 0, 0]

	buf = []u8{len: 8}
	little_endian_put_u32_at(mut buf, 0xfdf2e68f, 1)
	assert buf == [u8(0), 0x8f, 0xe6, 0xf2, 0xfd, 0, 0, 0]
}

fn test_little_endian_put_u32_end() {
	mut buf := []u8{len: 8}
	little_endian_put_u32_end(mut buf, 0x872fea95)
	assert buf == [u8(0), 0, 0, 0, 0x95, 0xea, 0x2f, 0x87]

	buf = []u8{len: 8}
	little_endian_put_u32_end(mut buf, 1)
	assert buf == [u8(0), 0, 0, 0, 1, 0, 0, 0]

	buf = []u8{len: 8}
	little_endian_put_u32_end(mut buf, 0xfdf2e68f)
	assert buf == [u8(0), 0, 0, 0, 0x8f, 0xe6, 0xf2, 0xfd]
}

fn test_little_endian_u64() {
	assert little_endian_u64([u8(0), 0, 0, 0, 0, 0, 0, 0]) == u64(0)
	assert little_endian_u64([u8(5), 4, 9, 1, 7, 3, 6, 8]) == u64(0x0806030701090405)
	assert little_endian_u64([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) == u64(0x8f8e9f7f219ea2f8)
	assert little_endian_u64([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) != u64(0xf8a29e217f9f8e8f)
}

fn test_little_endian_u64_at() {
	assert little_endian_u64_at([u8(1), 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1],
		1) == u64(0)
	assert little_endian_u64_at([u8(0), 5, 4, 9, 1, 7, 3, 6, 8, 0, 0, 0, 0, 0, 0, 0],
		1) == u64(0x0806030701090405)
	assert little_endian_u64_at([u8(0), 0xf8, 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f, 0, 0, 0,
		0, 0, 0, 0], 1) == u64(0x8f8e9f7f219ea2f8)
	assert little_endian_u64_at([u8(0), 0xf8, 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f, 0, 0, 0,
		0, 0, 0, 0], 1) != u64(0xf8a29e217f9f8e8f)
}

fn test_little_endian_u64_end() {
	assert little_endian_u64_end([u8(1), 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0]) == u64(0)
	assert little_endian_u64_end([u8(0), 0, 0, 0, 0, 0, 0, 0, 5, 4, 9, 1, 7, 3, 6, 8]) == u64(0x0806030701090405)
	assert little_endian_u64_end([u8(0), 0, 0, 0, 0, 0, 0, 0, 0xf8, 0xa2, 0x9e, 0x21, 0x7f, 0x9f,
		0x8e, 0x8f]) == u64(0x8f8e9f7f219ea2f8)
	assert little_endian_u64_end([u8(0), 0, 0, 0, 0, 0, 0, 0, 0xf8, 0xa2, 0x9e, 0x21, 0x7f, 0x9f,
		0x8e, 0x8f]) != u64(0xf8a29e217f9f8e8f)
}

fn test_little_endian_put_u64() {
	mut buf := []u8{len: 8}
	little_endian_put_u64(mut buf, 0x872fea95fdf2e68f)
	assert buf == [u8(0x8f), 0xe6, 0xf2, 0xfd, 0x95, 0xea, 0x2f, 0x87]
	little_endian_put_u64(mut buf, 0)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0]
	little_endian_put_u64(mut buf, 0xfdf2e68f8e9f7f21)
	assert buf == [u8(0x21), 0x7f, 0x9f, 0x8e, 0x8f, 0xe6, 0xf2, 0xfd]
}

fn test_little_endian_put_u64_at() {
	mut buf := []u8{len: 16}
	little_endian_put_u64_at(mut buf, 0x872fea95fdf2e68f, 1)
	assert buf == [u8(0), 0x8f, 0xe6, 0xf2, 0xfd, 0x95, 0xea, 0x2f, 0x87, 0, 0, 0, 0, 0, 0, 0]

	buf = []u8{len: 16}
	little_endian_put_u64_at(mut buf, 1, 1)
	assert buf == [u8(0), 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

	buf = []u8{len: 16}
	little_endian_put_u64_at(mut buf, 0xfdf2e68f8e9f7f21, 1)
	assert buf == [u8(0), 0x21, 0x7f, 0x9f, 0x8e, 0x8f, 0xe6, 0xf2, 0xfd, 0, 0, 0, 0, 0, 0, 0]
}

fn test_little_endian_put_u64_end() {
	mut buf := []u8{len: 16}
	little_endian_put_u64_end(mut buf, 0x872fea95fdf2e68f)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0, 0x8f, 0xe6, 0xf2, 0xfd, 0x95, 0xea, 0x2f, 0x87]

	buf = []u8{len: 16}
	little_endian_put_u64_end(mut buf, 1)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0]

	buf = []u8{len: 16}
	little_endian_put_u64_end(mut buf, 0xfdf2e68f8e9f7f21)
	assert buf == [u8(0), 0, 0, 0, 0, 0, 0, 0, 0x21, 0x7f, 0x9f, 0x8e, 0x8f, 0xe6, 0xf2, 0xfd]
}
