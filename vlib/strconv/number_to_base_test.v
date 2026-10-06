import strconv

fn test_format_int() {
	assert strconv.format_int(0, 2) == '0'
	assert strconv.format_int(0, 10) == '0'
	assert strconv.format_int(0, 16) == '0'
	assert strconv.format_int(0, 36) == '0'
	assert strconv.format_int(1, 2) == '1'
	assert strconv.format_int(1, 10) == '1'
	assert strconv.format_int(1, 16) == '1'
	assert strconv.format_int(1, 36) == '1'
	assert strconv.format_int(-1, 2) == '-1'
	assert strconv.format_int(-1, 10) == '-1'
	assert strconv.format_int(-1, 16) == '-1'
	assert strconv.format_int(-1, 36) == '-1'
	assert strconv.format_int(255, 2) == '11111111'
	assert strconv.format_int(255, 8) == '377'
	assert strconv.format_int(255, 10) == '255'
	assert strconv.format_int(255, 16) == 'ff'
	assert strconv.format_int(-255, 2) == '-11111111'
	assert strconv.format_int(-255, 8) == '-377'
	assert strconv.format_int(-255, 10) == '-255'
	assert strconv.format_int(-255, 16) == '-ff'
	for i in -256 .. 256 {
		assert strconv.format_int(i, 10) == i.str()
	}
}

fn test_format_uint() {
	assert strconv.format_uint(0, 2) == '0'
	assert strconv.format_int(255, 2) == '11111111'
	assert strconv.format_int(255, 8) == '377'
	assert strconv.format_int(255, 10) == '255'
	assert strconv.format_int(255, 16) == 'ff'
	assert strconv.format_uint(18446744073709551615, 2) == '1111111111111111111111111111111111111111111111111111111111111111'
	assert strconv.format_uint(18446744073709551615, 16) == 'ffffffffffffffff'
	assert strconv.format_uint(683058467, 36) == 'baobab'
}

fn test_format_int_min_i64() {
	assert strconv.format_int(min_i64, 2) == '-1000000000000000000000000000000000000000000000000000000000000000'
	assert strconv.format_int(min_i64, 8) == '-1000000000000000000000'
	assert strconv.format_int(min_i64, 10) == '-9223372036854775808'
	assert strconv.format_int(min_i64, 16) == '-8000000000000000'
	assert strconv.format_int(min_i64, 36) == '-1y2p0ij32e8e8'
	for radix in 2 .. 37 {
		for value in [min_i64, min_i64 + 1, max_i64] {
			assert strconv.parse_int(strconv.format_int(value, radix), radix, 64)! == value
		}
	}
}
