import strconv

fn test_atoi() {
	assert strconv.atoi('16')! == 16
	assert strconv.atoi('+16')! == 16
	assert strconv.atoi('-16')! == -16

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

fn test_parse_int() {
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

fn test_common_parse_uint2_compatibility() {
    test_list := [
        "1234,1234",
        "1_234,1234",
        "1_2_34,1234",
        "_12__34,1",
        "12__34,1",
        "_1234,1",
        "1234_,1",
        "0x1234,4660",
        "0x_1234,4660",
        "0x1_234,4660",
        "0x1_2_3_4,4660",
        "0_x1234,1",
        "0x1234_,1",
        "0o1234,668",
        "0o_1234,668",
        "0o1_234,668",
        "0o1_2_3_4,668",
        "0_o1234,1",
        "0o1234_,1",
        "0b111,7",
        "0b_111,7",
        "0b1_11,7",
        "0b1_1_1,7",
        "0_b111,1",
        "0b111_,1",

        "0xa,10",
        "0xA,10",
        "0xf,15",
        "0xf,15",

        "0_xf,1",
        "0x_0_0_f_,1",
        "0x_0_0__f,1",
        "0x_0_0_f,15",
    ]

    for tst in test_list {
        query := tst.split(",")
        mut a0 := strconv.common_parse_uint(query[0], 0, 32, true, true) or { 1 }
        //println("${a0} => ${query[1]}")
        assert a0.str() == query[1]
    }
}