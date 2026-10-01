module protobuf

fn test_put_fixed32_is_little_endian() {
	mut buf := []u8{}
	put_fixed32(mut buf, 0x01020304)
	assert buf == [u8(0x04), u8(0x03), u8(0x02), u8(0x01)]
}

fn test_put_fixed64_is_little_endian() {
	mut buf := []u8{}
	put_fixed64(mut buf, 0x0102030405060708)
	assert buf == [u8(0x08), u8(0x07), u8(0x06), u8(0x05), u8(0x04), u8(0x03), u8(0x02), u8(0x01)]
}

fn test_get_fixed32_known_encoding() {
	value, pos := get_fixed32([u8(0x04), u8(0x03), u8(0x02), u8(0x01)], 0)!
	assert value == 0x01020304
	assert pos == 4
}

fn test_get_fixed64_known_encoding() {
	data := [u8(0x08), u8(0x07), u8(0x06), u8(0x05), u8(0x04), u8(0x03), u8(0x02), u8(0x01)]
	value, pos := get_fixed64(data, 0)!
	assert value == 0x0102030405060708
	assert pos == 8
}

fn test_fixed_round_trip() {
	values := [u32(0), 1, 0x7f, 0x80, 0xff, 0xffff, 0x7fff_ffff, 0x8000_0000, u32(0xffff_ffff)]
	for value in values {
		mut buf := []u8{}
		put_fixed32(mut buf, value)
		got, pos := get_fixed32(buf, 0)!
		assert got == value, 'fixed32 ${value} came back as ${got}'
		assert pos == 4
	}
	bigs := [u64(0), 1, 0xffff_ffff, 0x1_0000_0000, 0x8000_0000_0000_0000, u64(0xffff_ffff_ffff_ffff)]
	for value in bigs {
		mut buf := []u8{}
		put_fixed64(mut buf, value)
		got, pos := get_fixed64(buf, 0)!
		assert got == value, 'fixed64 ${value} came back as ${got}'
		assert pos == 8
	}
}

fn test_get_fixed32_at_offset() {
	data := [u8(0xaa), u8(0xbb), u8(0x04), u8(0x03), u8(0x02), u8(0x01)]
	value, pos := get_fixed32(data, 2)!
	assert value == 0x01020304
	assert pos == 6
}

fn test_get_fixed32_truncated_fails() {
	if got, _ := get_fixed32([u8(0x01), u8(0x02), u8(0x03)], 0) {
		assert false, 'expected a truncated fixed32 to fail, got ${got}'
	} else {
		assert err is UnexpectedEofError
	}
}

fn test_get_fixed64_truncated_fails() {
	if got, _ := get_fixed64([u8(0x01), u8(0x02), u8(0x03), u8(0x04), u8(0x05), u8(0x06), u8(0x07)], 0) {
		assert false, 'expected a truncated fixed64 to fail, got ${got}'
	} else {
		assert err is UnexpectedEofError
	}
}

fn test_put_float_known_bit_pattern() {
	// 1.0f is 0x3f800000, little-endian.
	mut buf := []u8{}
	put_float(mut buf, f32(1.0))
	assert buf == [u8(0x00), u8(0x00), u8(0x80), u8(0x3f)]

	// 0.5f is 0x3f000000.
	buf = []u8{}
	put_float(mut buf, f32(0.5))
	assert buf == [u8(0x00), u8(0x00), u8(0x00), u8(0x3f)]

	// -2.0f is 0xc0000000.
	buf = []u8{}
	put_float(mut buf, f32(-2.0))
	assert buf == [u8(0x00), u8(0x00), u8(0x00), u8(0xc0)]
}

fn test_put_double_known_bit_pattern() {
	// 1.0 is 0x3ff0000000000000, little-endian.
	mut buf := []u8{}
	put_double(mut buf, f64(1.0))
	assert buf == [u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0xf0), u8(0x3f)]

	// -0.5 is 0xbfe0000000000000.
	buf = []u8{}
	put_double(mut buf, f64(-0.5))
	assert buf == [u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0xe0), u8(0xbf)]
}

fn test_float_round_trip() {
	values := [f32(0.0), 1.0, -1.0, 0.5, -0.5, 3.14159, 1e30, -1e-30]
	for value in values {
		mut buf := []u8{}
		put_float(mut buf, value)
		assert buf.len == 4
		got, pos := get_float(buf, 0)!
		assert got == value, 'float ${value} came back as ${got}'
		assert pos == 4
	}
}

fn test_double_round_trip() {
	values := [f64(0.0), 1.0, -1.0, 0.5, -0.5, 3.141592653589793, 1e300, -1e-300]
	for value in values {
		mut buf := []u8{}
		put_double(mut buf, value)
		assert buf.len == 8
		got, pos := get_double(buf, 0)!
		assert got == value, 'double ${value} came back as ${got}'
		assert pos == 8
	}
}

fn test_float_nan_and_infinity_survive() {
	// The bit pattern is what travels, so these must round trip unchanged.
	// The values are built from bits because this compiler exposes no infinity
	// or NaN literal, and the test is about the bits anyway.
	pos_inf := unsafe { U32F32{ u: 0x7f80_0000 }.f }
	neg_inf := unsafe { U64F64{ u: 0xfff0_0000_0000_0000 }.f }

	mut buf := []u8{}
	put_float(mut buf, pos_inf)
	assert buf == [u8(0x00), u8(0x00), u8(0x80), u8(0x7f)]
	got, _ := get_float(buf, 0)!
	assert got == pos_inf
	// An infinite float is still exactly as wide as a finite one.
	assert buf.len == 4

	buf = []u8{}
	put_double(mut buf, neg_inf)
	assert buf == [u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0xf0), u8(0xff)]
	gotd, _ := get_double(buf, 0)!
	assert gotd == neg_inf
}

fn test_nan_survives_as_a_bit_pattern() {
	// NaN never compares equal to itself, so only the bytes can be checked.
	nan_bits := u32(0x7fc0_0000)
	mut buf := []u8{}
	put_float(mut buf, unsafe { U32F32{ u: nan_bits }.f })
	assert buf == [u8(0x00), u8(0x00), u8(0xc0), u8(0x7f)]
	back, _ := get_float(buf, 0)!
	assert unsafe { U32F32{ f: back }.u } == nan_bits
}

fn test_negative_zero_is_preserved() {
	// -0.0 and 0.0 compare equal, so only the bytes can tell them apart.
	mut pos_buf := []u8{}
	mut neg_buf := []u8{}
	put_float(mut pos_buf, f32(0.0))
	put_float(mut neg_buf, unsafe { U32F32{ u: 0x8000_0000 }.f })
	assert pos_buf == [u8(0x00), u8(0x00), u8(0x00), u8(0x00)]
	assert neg_buf == [u8(0x00), u8(0x00), u8(0x00), u8(0x80)]
	assert pos_buf != neg_buf
}
