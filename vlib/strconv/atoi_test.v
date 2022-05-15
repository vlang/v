import strconv

fn test_atoi() ? {
	assert strconv.atoi('16')? == 16
	assert strconv.atoi('+16')? == 16
	assert strconv.atoi('-16')? == -16

	// invalid strings
	if x := strconv.atoi('str') {
		println(x)
		assert false
	} else {
		assert true
	}
	if x := strconv.atoi('string_longer_than_10_chars') {
		println(x)
		assert false
	} else {
		assert true
	}
	if x := strconv.atoi('') {
		println(x)
		assert false
	} else {
		assert true
	}
}

fn test_parse_int() ? {
	// Different bases
	assert strconv.parse_int('16', 16, 0)? == 0x16
	assert strconv.parse_int('16', 8, 0)? == 0o16
	assert strconv.parse_int('11', 2, 0)? == 3
	// Different bit sizes
	assert strconv.parse_int('127', 10, 8)? == 127
	assert strconv.parse_int('128', 10, 8)? == 127
	assert strconv.parse_int('32767', 10, 16)? == 32767
	assert strconv.parse_int('32768', 10, 16)? == 32767
	assert strconv.parse_int('2147483647', 10, 32)? == 2147483647
	assert strconv.parse_int('2147483648', 10, 32)? == 2147483647
	assert strconv.parse_int('9223372036854775807', 10, 64)? == 9223372036854775807
	assert strconv.parse_int('9223372036854775808', 10, 64)? == 9223372036854775807
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
