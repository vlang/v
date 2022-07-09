module binary

// Little Endian Tests

fn test_little_endian_u16() {
	assert little_endian_u16([u8(0), 0]) == u16(0)
	assert little_endian_u16([u8(5), 4]) == u16(0x0405)
	assert little_endian_u16([u8(0x35), 0x57]) == u16(0x5735)
	assert little_endian_u16([u8(0x35), 0x57]) != u16(0x3557)
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

fn test_little_endian_u32() {
	assert little_endian_u32([u8(0), 0, 0, 0]) == u32(0)
	assert little_endian_u32([u8(5), 4, 9, 1]) == u32(0x01090405)
	assert little_endian_u32([u8(0xf8), 0xa2, 0x9e, 0x21]) == u32(0x219ea2f8)
	assert little_endian_u32([u8(0xf8), 0xa2, 0x9e, 0x21]) != u32(0xf8a29e21)
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

fn test_little_endian_u64() {
	assert little_endian_u64([u8(0), 0, 0, 0, 0, 0, 0, 0]) == u64(0)
	assert little_endian_u64([u8(5), 4, 9, 1, 7, 3, 6, 8]) == u64(0x0806030701090405)
	assert little_endian_u64([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) == u64(0x8f8e9f7f219ea2f8)
	assert little_endian_u64([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) != u64(0xf8a29e217f9f8e8f)
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

// Big Endian Tests

fn test_big_endian_u16() {
	assert big_endian_u16([u8(0), 0]) == u16(0)
	assert big_endian_u16([u8(5), 4]) == u16(0x0504)
	assert big_endian_u16([u8(0x35), 0x57]) == u16(0x3557)
	assert big_endian_u16([u8(0x35), 0x57]) != u16(0x5735)
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

fn test_big_endian_u32() {
	assert big_endian_u32([u8(0), 0, 0, 0]) == u32(0)
	assert big_endian_u32([u8(5), 4, 9, 1]) == u32(0x05040901)
	assert big_endian_u32([u8(0xf8), 0xa2, 0x9e, 0x21]) == u32(0xf8a29e21)
	assert big_endian_u32([u8(0xf8), 0xa2, 0x9e, 0x21]) != u32(0x2192a2f8)
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

fn test_big_endian_u64() {
	assert big_endian_u64([u8(0), 0, 0, 0, 0, 0, 0, 0]) == u64(0)
	assert big_endian_u64([u8(5), 4, 9, 1, 7, 3, 6, 8]) == u64(0x0504090107030608)
	assert big_endian_u64([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) == u64(0xf8a29e217f9f8e8f)
	assert big_endian_u64([u8(0xf8), 0xa2, 0x9e, 0x21, 0x7f, 0x9f, 0x8e, 0x8f]) != u64(0x8f8e9f7f219ea2f8)
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
