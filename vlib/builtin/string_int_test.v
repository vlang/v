import strconv

fn test_common_atoi() {
	// test common cases
	assert '70zzz'.int() == 70
	assert '2901issue'.int() == 2901
	assert '234232w'.int() == 234232
	assert '-9009x'.int() == -9009
	assert '0y'.int() == 0

	// test lead zeros
	assert '0000012'.int() == 12
	assert '-0000012'.int() == -12
	assert '0x001F'.int() == 31
	assert '-0x001F'.int() == -31
	assert '0x001f'.int() == 31
	assert '0o00011'.int() == 9
	assert '0b00001001'.int() == 9

	// test underscore in string
	assert '-10_000'.int() == -10000
	assert '-0x00_0_f_ff'.int() == -0xfff
	assert '10_000_000'.int() == 10000000

	for n in -10000 .. 100000 {
		s := n.str() + 'z'
		assert s.int() == n
	}
}

fn test_unsigned_cast() {
	// tests for u16

	// test common cases
	assert '70zzz'.u16() == 70
	assert '2901issue'.u16() == 2901
	assert '0y'.u16() == 0

	// test lead zeros
	assert '0000012'.u16() == 12
	assert '0x001F'.u16() == 31
	assert '0x001f'.u16() == 31
	assert '0o00011'.u16() == 9
	assert '0b00001001'.u16() == 9

	// tests for u32

	// test common cases
	assert '70zzz'.u32() == 70
	assert '2901issue'.u32() == 2901
	assert '234232w'.u32() == 234232
	assert '-9009x'.u32() == 0
	assert '0y'.u32() == 0

	// test lead zeros
	assert '0000012'.u32() == 12
	assert '-0000012'.u32() == 0
	assert '0x001F'.u32() == 31
	assert '-0x001F'.u32() == 0
	assert '0x001f'.u32() == 31
	assert '0o00011'.u32() == 9
	assert '0b00001001'.u32() == 9

	// test underscore in string
	assert '-10_000'.u32() == 0
	assert '-0x00_0_f_ff'.u32() == 0
	assert '10_000_000'.u32() == 10000000

	for n in 0 .. u32(100) {
		s := n.str() + 'z'
		assert s.u32() == n
	}

	// tests for u64

	// test common cases
	assert '70zzz'.u64() == 70
	assert '2901issue'.u64() == 2901
	assert '234232w'.u64() == 234232
	assert '-9009x'.u64() == 0
	assert '0y'.u64() == 0

	// test lead zeros
	assert '0000012'.u64() == 12
	assert '-0000012'.u64() == 0
	assert '0x001F'.u64() == 31
	assert '-0x001F'.u64() == 0
	assert '0x001f'.u64() == 31
	assert '0o00011'.u64() == 9
	assert '0b00001001'.u64() == 9

	// test underscore in string
	assert '-10_000'.u64() == 0
	assert '-0x00_0_f_ff'.u64() == 0
	assert '10_000_000'.u64() == 10000000

	for n in 0 .. u64(10000) {
		s := n.str() + 'z'
		assert s.u64() == n
	}
}

fn test_signed_cast() {
	// tests for i64

	// test common cases
	assert '70zzz'.i64() == 70
	assert '2901issue'.i64() == 2901
	assert '234232w'.i64() == 234232
	assert '-9009x'.i64() == -9009
	assert '0y'.i64() == 0

	// test lead zeros
	assert '0000012'.i64() == 12
	assert '-0000012'.i64() == -12
	assert '0x001F'.i64() == 31
	assert '-0x001F'.i64() == -31
	assert '0x001f'.i64() == 31
	assert '0o00011'.i64() == 9
	assert '0b00001001'.i64() == 9

	// test underscore in string
	assert '-10_000'.i64() == -10000
	assert '-0x00_0_f_ff'.i64() == -0xfff
	assert '10_000_000'.i64() == 10000000

	for n in -10000 .. 100000 {
		s := n.str() + 'z'
		assert s.i64() == n
	}

	// tests for i8

	// test common cases
	assert '70zzz'.i8() == 70
	assert '29issue'.i8() == 29
	assert '22w'.i8() == 22
	assert '-90x'.i8() == -90
	assert '0y'.i8() == 0

	// test lead zeros
	assert '0000012'.i8() == 12
	assert '-0000012'.i8() == -12
	assert '0x001F'.i8() == 31
	assert '-0x001F'.i8() == -31
	assert '0x001f'.i8() == 31
	assert '0o00011'.i8() == 9
	assert '0b000011'.i8() == 3

	// test underscore in string
	assert '-10_0'.i8() == -100
	assert '-0x0_0_f'.i8() == -0xf
	assert '10_0'.i8() == 100

	for n in -10 .. 100 {
		s := n.str() + 'z'
		assert s.i8() == n
	}

	// tests for i16

	// test common cases
	assert '70zzz'.i16() == 70
	assert '2901issue'.i16() == 2901
	assert '2342w'.i16() == 2342
	assert '-9009x'.i16() == -9009
	assert '0y'.i16() == 0

	// test lead zeros
	assert '0000012'.i16() == 12
	assert '-0000012'.i16() == -12
	assert '0x001F'.i16() == 31
	assert '-0x001F'.i16() == -31
	assert '0x001f'.i16() == 31
	assert '0o00011'.i16() == 9
	assert '0b00001001'.i16() == 9

	// test underscore in string
	assert '-10_0'.i16() == -100
	assert '-0x00_0_fff'.i16() == -0xfff
	assert '10_0'.i16() == 100

	for n in -100 .. 100 {
		s := n.str() + 'z'
		assert s.i16() == n
	}

	// test g format
	unsafe {
		mut u := strconv.Float64u{
			u: strconv.double_plus_zero
		}
		assert '${u.f:g}' == '0.0'
		assert '${u.f:G}' == '0.0'
		u.u = strconv.double_minus_zero
		assert '${u.f:g}' == '0.0'
		assert '${u.f:G}' == '0.0'
		u.u = strconv.double_plus_infinity
		assert '${u.f:g}' == '+inf'
		assert '${u.f:G}' == '+INF'
		u.u = strconv.double_minus_infinity
		assert '${u.f:g}' == '-inf'
		assert '${u.f:G}' == '-INF'
	}
	unsafe {
		mut u := strconv.Float32u{
			u: strconv.single_plus_zero
		}
		assert '${u.f:g}' == '0.0'
		assert '${u.f:G}' == '0.0'
		u.u = strconv.single_minus_zero
		assert '${u.f:g}' == '0.0'
		assert '${u.f:G}' == '0.0'
		u.u = strconv.single_plus_infinity
		assert '${u.f:g}' == '+inf'
		assert '${u.f:G}' == '+INF'
		u.u = strconv.single_minus_infinity
		assert '${u.f:g}' == '-inf'
		assert '${u.f:G}' == '-INF'
	}
}

fn test_binary() {
	i := i8(127)
	u := u8(127)
	assert '${i:08b}' == '01111111'
	assert '${u:08b}' == '01111111'
	assert '${i16(i):08b}' == '01111111'
	assert '${u16(u):08b}' == '01111111'
	assert '${int(i):08b}' == '01111111'
	assert '${u32(u):08b}' == '01111111'
	assert '${i64(i):08b}' == '01111111'
	assert '${u64(u):08b}' == '01111111'

	n := i8(-1)
	assert '${u8(-1):08b}' == '11111111'
	assert '${u16(n):08b}' == '1111111111111111'
	assert '${u32(n):08b}' == '11111111111111111111111111111111'
	assert '${u64(n):08b}' == '1111111111111111111111111111111111111111111111111111111111111111'
}

fn test_binary32() {
	i := int(0x7fff_ffff)
	u := u32(0x7fff_ffff)
	assert '${i:032b}' == '01111111111111111111111111111111'
	assert '${u:032b}' == '01111111111111111111111111111111'
	assert '${i64(i):032b}' == '01111111111111111111111111111111'
	assert '${u64(u):032b}' == '01111111111111111111111111111111'
}

fn test_binary64() {
	i := i64(0x7fff_ffff_ffff_ffff)
	u := u64(0x7fff_ffff_ffff_ffff)
	assert '${i:064b}' == '0111111111111111111111111111111111111111111111111111111111111111'
	assert '${u:064b}' == '0111111111111111111111111111111111111111111111111111111111111111'
}

fn test_interpolation_of_negative_numbers_padding_and_width() {
	a := -77
	assert '                 -77' == '${a:20}'
	assert '                 -77' == '${a:20d}'
	assert '                 -4d' == '${a:20x}'
	assert '            -1001101' == '${a:20b}'

	assert '-0000000000000000077' == '${a:020}'
	assert '-0000000000000000077' == '${a:020d}'
	assert '-000000000000000004d' == '${a:020x}'
	assert '-0000000000001001101' == '${a:020b}'

	//
	assert '     -77' == '${a:8}'
	assert '     -77' == '${a:8d}'
	assert '     -4d' == '${a:8x}'
	assert '-1001101' == '${a:8b}'

	assert '-0000077' == '${a:08}'
	assert '-0000077' == '${a:08d}'
	assert '-1001101' == '${a:08b}'
	assert '-000004d' == '${a:08x}'

	//
	assert ' -77' == '${a:4}'
	assert ' -77' == '${a:4d}'
	assert '-1001101' == '${a:4b}'
	assert ' -4d' == '${a:4x}'

	assert '-077' == '${a:04}'
	assert '-077' == '${a:04d}'
	assert '-1001101' == '${a:04b}'
	assert '-04d' == '${a:04x}'

	//
	assert '-77' == '${a:2}'
	assert '-77' == '${a:2d}'
	assert '-1001101' == '${a:2b}'
	assert '-4d' == '${a:2x}'

	assert '-77' == '${a:02}'
	assert '-77' == '${a:02d}'
	assert '-1001101' == '${a:02b}'
	assert '-4d' == '${a:02x}'

	//
	bin0 := ~6
	assert bin0 == -7
	assert '-0000111' == '${bin0:08b}' // a minimum of 8 characters for the whole number, including the padding and the sign
	assert '-0000111' == '${~6:08b}'
	assert '    -111' == '${~6:8b}'

	//
	assert '-0000110' == '${-6:08b}'
	assert '    -110' == '${-6:8b}'
}

fn test_parse() {
	assert i64(1) == '1'.parse_int(0, 8) or { 0 }
	assert i64(1) == '0b01'.parse_int(0, 8) or { 0 }
	assert i64(1) == '01'.parse_int(0, 8) or { 0 }
	assert i64(1) == '0o01'.parse_int(0, 8) or { 0 }
	assert i64(1) == '0x01'.parse_int(0, 8) or { 0 }
	assert i64(1) == '1'.parse_int(2, 8) or { 0 }
	assert i64(1) == '1'.parse_int(8, 8) or { 0 }
	assert i64(1) == '1'.parse_int(10, 8) or { 0 }
	assert i64(1) == '1'.parse_int(16, 8) or { 0 }

	assert u64(1) == '1'.parse_uint(0, 8) or { 0 }
	assert u64(1) == '0b01'.parse_uint(0, 8) or { 0 }
	assert u64(1) == '01'.parse_uint(0, 8) or { 0 }
	assert u64(1) == '0o01'.parse_uint(0, 8) or { 0 }
	assert u64(1) == '0x01'.parse_uint(0, 8) or { 0 }
	assert u64(1) == '1'.parse_uint(2, 8) or { 0 }
	assert u64(1) == '1'.parse_uint(8, 8) or { 0 }
	assert u64(1) == '1'.parse_uint(10, 8) or { 0 }
	assert u64(1) == '1'.parse_uint(16, 8) or { 0 }
}
