import strconv

fn test_atoi() {
	assert strconv.atoi('16') == 16
	assert strconv.atoi('+16') == 16
	assert strconv.atoi('-16') == -16
	assert strconv.atoi('str') == 0
	assert strconv.atoi('') == 0
}

fn test_parse_int() {
	// Different bases
	assert strconv.parse_int('16', 16, 0) == 0x16
	assert strconv.parse_int('16', 8, 0) == 0o16
	assert strconv.parse_int('11', 2, 0) == 3
	// Different bit sizes
	assert strconv.parse_int('127', 10, 8) == 127
	assert strconv.parse_int('128', 10, 8) == 127
	assert strconv.parse_int('32767', 10, 16) == 32767
	assert strconv.parse_int('32768', 10, 16) == 32767
	assert strconv.parse_int('2147483647', 10, 32) == 2147483647
	assert strconv.parse_int('2147483648', 10, 32) == 2147483647
	assert strconv.parse_int('9223372036854775807', 10, 64) == 9223372036854775807
	assert strconv.parse_int('9223372036854775808', 10, 64) == 9223372036854775807
	// Invalid bit sizes
	assert strconv.parse_int('123', 10, 65) == 0
	assert strconv.parse_int('123', 10, -1) == 0
}
