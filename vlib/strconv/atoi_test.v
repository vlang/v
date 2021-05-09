import strconv

fn test_atoi() {
	if x := strconv.atoi('16') {
		assert x == 16
	} else {
		assert false
	}
	if x := strconv.atoi('+16') {
		assert x == 16
	} else {
		assert false
	}
	if x := strconv.atoi('-16') {
		assert x == -16
	} else {
		assert false
	}
	if x := strconv.atoi('str') {
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

fn parse_int_or_zero(s string, base int, bits int) i64 {
	return strconv.parse_int(s, base, bits) or { 0 }
}

fn test_parse_int() {
	// Different bases
	assert parse_int_or_zero('16', 16, 0) == 0x16
	assert parse_int_or_zero('16', 8, 0) == 0o16
	assert parse_int_or_zero('11', 2, 0) == 3
	// Different bit sizes
	assert parse_int_or_zero('127', 10, 8) == 127
	assert parse_int_or_zero('128', 10, 8) == 0
	assert parse_int_or_zero('32767', 10, 16) == 32767
	assert parse_int_or_zero('32768', 10, 16) == 0
	assert parse_int_or_zero('2147483647', 10, 32) == 2147483647
	assert parse_int_or_zero('2147483648', 10, 32) == 0
	assert parse_int_or_zero('9223372036854775807', 10, 64) == 9223372036854775807
	assert parse_int_or_zero('9223372036854775808', 10, 64) == 0
	// Invalid bit sizes
	assert parse_int_or_zero('123', 10, 65) == 0
	assert parse_int_or_zero('123', 10, -1) == 0
}

fn test_common_parse_uint() ? {
	r1 := strconv.common_parse_uint('1', 10, 8, false) ?
	assert r1 == 1
	r2 := strconv.common_parse_uint('123', 10, 8, false) ?
	assert r2 == 123
	if result := strconv.common_parse_uint('123', 10, 65, false) {
		assert false
	}
	if result := strconv.common_parse_uint('123', 10, -1, false) {
		assert false
	}
	if result := strconv.common_parse_uint('', 10, 8, false) {
		assert false
	}
	if result := strconv.common_parse_uint('1a', 10, 8, false) {
		println(result)
		assert false
	}
	if result := strconv.common_parse_uint('12a', 10, 8, false) {
		assert false
	}
	if result := strconv.common_parse_uint('123a', 10, 8, false) {
		assert false
	}
}
