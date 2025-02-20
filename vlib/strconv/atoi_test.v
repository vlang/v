import strconv

struct StrInt { // test struct
	str_value string
	int_value int
}

// test what should be caught by atoi_common_check
fn test_common_check() {
	// Parsing of these strings should fail on all types.
	ko := [
		'', // Empty string
		'-', // Only sign
		'+', // Only sign
		'_', // Only Underscore
		'_10', // Start with underscore
		'+_10', // Start with underscore after sign.
		'-_16', // Start with underscore after sign.
		'123_', // End with underscore
	]

	for v in ko {
		if r := strconv.atoi(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string "${v}" should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for : "${v}')
			assert true
		}
	}
}

// Test things accepted, and rejected in atoi_common function.
fn test_atoi_common() {
	// Parsing of theses value should succeed on all types.
	ok := [
		StrInt{'1', 1},
		StrInt{'-1', -1},
		StrInt{'0', 0},
		StrInt{'+0', 0},
		StrInt{'-0', 0},
		StrInt{'-0_00', 0},
		StrInt{'+0_00', 0},
		StrInt{'+1', 1},
		StrInt{'+123', 123},
		StrInt{'-1_2_1', -121},
		StrInt{'00000006', 6},
		StrInt{'0_0_0_0_0_0_0_6', 6},
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atoi(v.str_value)! == v.int_value
	}

	ko := [// Parsing of these strings should fail on all types.
		'-3__1', // Two consecutives underscore.
		'-3_1A', // Non radix 10 char.
		'A42', // Non radix 10 char.
	]

	for v in ko {
		if r := strconv.atoi(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} int extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for : "${v}')
			assert true
		}
	}
}

// performs numeric (bounds) tests over int type.
fn test_atoi() {
	ok := [
		StrInt{'1', 1},
		StrInt{'-1', -1},
		StrInt{'0', 0},
		StrInt{'+3_14159', 314159},
		StrInt{'-1_00_1', -1001},
		StrInt{'-1_024', -1024},
		StrInt{'123_456_789', 123456789},
		StrInt{'00000006', 6},
		StrInt{'0_0_0_0_0_0_0_6', 6},
		StrInt{'2147483647', max_int},
		StrInt{'-2147483648', min_int},
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atoi(v.str_value)! == v.int_value
	}

	// Parsing of these values should fail !
	ko := [
		'-2147483649', // 32bits underflow by 1.
		'+2147483648', // 32 bit overflow by 1.
		'+3147483648', // 32 bit overflow by a lot.
		'-2147244836470', // Large underflow.
		'+86842255899621148766244',
	]

	for v in ko {
		if r := strconv.atoi(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} int extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for : "${v}')
			assert true
		}
	}
}

// performs numeric (bounds) tests over i8 type.
fn test_atoi8() {
	struct StrI8 { // Inner test struct
		str_value string
		int_value i8
	}

	ok := [
		StrI8{'0', 0}, // All kind of zeroes
		StrI8{'+0', 0},
		StrI8{'-0', 0},
		StrI8{'-0_00', 0},
		StrI8{'+0_00', 0},
		StrI8{'1', 1},
		StrI8{'+1', 1},
		StrI8{'-1', -1},
		StrI8{'+123', 123},
		StrI8{'-1_2_1', -121},
		StrI8{'0_0_0_0_0_0_0_6', 6},
		StrI8{'127', max_i8},
		StrI8{'-128', min_i8},
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atoi8(v.str_value)! == v.int_value
	}

	// Parsing of these values should fail !
	ko := [
		'-129', // i8 bits underflow by 1.
		'+128', // i8 bit overflow by 1.
		'+256', // i8 overflow with value equal to max u8.
		'+3147483648', // i8 bit overflow by a lot.
		'-4836470', // Large i8 underflow.
	]

	for v in ko {
		if r := strconv.atoi8(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} int extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for : "${v}')
			assert true
		}
	}
}

// performs numeric (bounds) tests over i16 type.
fn test_atoi16() {
	struct StrI16 { // Inner test struct
		str_value string
		int_value i16
	}

	ok := [
		StrI16{'0', 0}, // All kind of zeroes
		StrI16{'+0', 0},
		StrI16{'-0', 0},
		StrI16{'-0_00', 0},
		StrI16{'+0_00', 0},
		StrI16{'1', 1},
		StrI16{'+1', 1},
		StrI16{'-1', -1},
		StrI16{'+123', 123},
		StrI16{'-1_2_1', -121},
		StrI16{'0_0_0_0_0_0_0_6', 6},
		StrI16{'32767', max_i16},
		StrI16{'-32768', min_i16},
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atoi16(v.str_value)! == v.int_value
	}

	// Parsing of these values should fail !
	ko := [
		'-32769', // i16 bits underflow by 1.
		'+32768', // i16 bit overflow by 1.
		'+45_000', // i16 bit overflow by a lot.
		'65536', // i16 overflow with value equal to u16 max.
		'-483_647_909', // Large i16 underflow.
	]

	for v in ko {
		if r := strconv.atoi16(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} int extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for : "${v}')
			assert true
		}
	}
}

// performs numeric (bounds) tests over i32 type. This test is redundant with atoi
// which performs same on int (actually 32bits). In the future, int COULD be mapped
// on arch with (e.g 64bits). That's why this test exists.
fn test_atoi32() {
	struct StrI32 { // Inner test struct
		str_value string
		int_value i32
	}

	ok := [
		StrI32{'0', 0}, // All kind of zeroes
		StrI32{'+0', 0},
		StrI32{'-0', 0},
		StrI32{'-0_00', 0},
		StrI32{'+0_00', 0},
		StrI32{'1', 1},
		StrI32{'+1', 1},
		StrI32{'-1', -1},
		StrI32{'+123', 123},
		StrI32{'-1_2_1', -121},
		StrI32{'0_0_0_0_0_0_0_6', 6},
		StrI32{'2147483647', max_i32},
		StrI32{'-2147483648', min_i32},
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atoi32(v.str_value)! == v.int_value
	}

	// Parsing of these values should fail !
	ko := [
		'-2147483649', // i32 bits underflow by 1.
		'+2147483648', // i32 bit overflow by 1.
		'+4294967295', // Large Overflow but equal to u32 max.
		'-483_647_909_912_754', // Large i32 underflow.
	]

	for v in ko {
		if r := strconv.atoi32(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} int extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for : "${v}')
			assert true
		}
	}
}

fn test_atoi64() {
	struct StrI64 { // Inner test struct
		str_value string
		int_value i64
	}

	ok := [
		StrI64{'0', 0}, // All kind of zeroes
		StrI64{'+0', 0},
		StrI64{'-0', 0},
		StrI64{'-0_00', 0},
		StrI64{'+0_00', 0},
		StrI64{'1', 1},
		StrI64{'+1', 1},
		StrI64{'-1', -1},
		StrI64{'+123', 123},
		StrI64{'-1_2_1', -121},
		StrI64{'0_0_0_0_0_0_0_6', 6},
		StrI64{'9223372036854775807', max_i64},
		StrI64{'-9223372036854775808', min_i64},
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atoi64(v.str_value)! == v.int_value
	}

	// Parsing of these values should fail !
	ko := [
		'-9223372036854775809', // i64 bits underflow by 1.
		'+9223372036854775808', // i64 bit overflow by 1.
		'+18446744073709551615', // Large Overflow but equal to u64 max.
		'-483647909912754123456789', // Large i64 underflow.
	]

	for v in ko {
		if r := strconv.atoi64(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} int extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for : "${v}')
			assert true
		}
	}
}

fn test_parse_int() {
	// symbols coverage
	assert strconv.parse_int('1234567890', 10, 32)! == 1234567890
	assert strconv.parse_int('19aAbBcCdDeEfF', 16, 64)! == 0x19aAbBcCdDeEfF
	// Different bases
	assert strconv.parse_int('16', 16, 0)! == 0x16
	assert strconv.parse_int('16', 8, 0)! == 0o16
	assert strconv.parse_int('11', 2, 0)! == 3
	// Different bit sizes
	assert strconv.parse_int('127', 10, 8)! == 127
	assert strconv.parse_int('128', 10, 8)! == 127
	assert strconv.parse_int('32767', 10, 16)! == 32767
	assert strconv.parse_int('32768', 10, 16)! == 32767
	assert strconv.parse_int('2147483647', 10, 32)! == 2147483647
	assert strconv.parse_int('2147483648', 10, 32)! == 2147483647
	assert strconv.parse_int('9223372036854775807', 10, 64)! == 9223372036854775807
	assert strconv.parse_int('9223372036854775808', 10, 64)! == 9223372036854775807
	assert strconv.parse_int('baobab', 36, 64)? == 683058467
	// Invalid bit sizes
	if x := strconv.parse_int('123', 10, -1) {
		println(x)
		assert false
	} else {
		assert true
	}
	if x := strconv.parse_int('123', 10, 65) {
		println(x)
		assert false
	} else {
		assert true
	}
}

fn test_common_parse_uint2() {
	mut result, mut error := strconv.common_parse_uint2('1', 10, 8)
	assert result == 1
	assert error == 0
	result, error = strconv.common_parse_uint2('123', 10, 8)
	assert result == 123
	assert error == 0
	result, error = strconv.common_parse_uint2('123', 10, 65)
	assert result == 0
	assert error == -2
	result, error = strconv.common_parse_uint2('123', 10, -1)
	assert result == 0
	assert error == -2
	result, error = strconv.common_parse_uint2('', 10, 8)
	assert result == 0
	assert error == 1
	result, error = strconv.common_parse_uint2('1a', 10, 8)
	assert result == 1
	assert error == 2
	result, error = strconv.common_parse_uint2('12a', 10, 8)
	assert result == 12
	assert error == 3
	result, error = strconv.common_parse_uint2('123a', 10, 8)
	assert result == 123
	assert error == 4
}

fn test_common_parse_uint2_fail() {
	mut ascii_characters := [' ', '!', '"', '#', '\$', '%', '&', "'", '(', ')', '*', '+', ',',
		'-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '_', '`', '{', '|',
		'}', '~']
	mut special_characters := [':', ';', '<', '=', '>', '?', '@', 'X', 'Y', 'Z', '[', '\\', ']',
		'^', '_', '`']

	num0, err0 := strconv.common_parse_uint2('1Ab', 16, 32)
	assert num0 == 427
	assert err0 == 0

	for ch in ascii_characters {
		// println("ch: [${ch}]")
		txt_str := '${ch[0]:c}12Ab'
		num, err := strconv.common_parse_uint2(txt_str, 16, 32)
		assert err != 0
	}

	for ch in special_characters {
		// println("ch: [${ch}]")
		txt_str := '${ch[0]:c}12Ab'
		num, err := strconv.common_parse_uint2(txt_str, 16, 32)
		assert err != 0
	}
}

fn test_common_parse_uint2_compatibility() {
	test_list := [
		'1234,1234',
		'1_234,1234',
		'1_2_34,1234',
		'_12__34,1',
		'12__34,1',
		'_1234,1',
		'1234_,1',
		'0x1234,4660',
		'0x_1234,4660',
		'0x1_234,4660',
		'0x1_2_3_4,4660',
		'0_x1234,1',
		'0x1234_,1',
		'0o1234,668',
		'0o_1234,668',
		'0o1_234,668',
		'0o1_2_3_4,668',
		'0_o1234,1',
		'0o1234_,1',
		'0b111,7',
		'0b_111,7',
		'0b1_11,7',
		'0b1_1_1,7',
		'0_b111,1',
		'0b111_,1',
		'0xa,10',
		'0xA,10',
		'0xf,15',
		'0xf,15',
		'0_xf,1',
		'0x_0_0_f_,1',
		'0x_0_0__f,1',
		'0x_0_0_f,15',
	]

	for tst in test_list {
		query := tst.split(',')
		mut a0 := strconv.common_parse_uint(query[0], 0, 32, true, true) or { 1 }
		// println("${a0} => ${query[1]}")
		assert a0.str() == query[1]
	}
}
