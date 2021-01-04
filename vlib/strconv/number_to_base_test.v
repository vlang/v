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
	assert strconv.format_uint(18446744073709551615, 2) ==
		'1111111111111111111111111111111111111111111111111111111111111111'
	assert strconv.format_uint(18446744073709551615, 16) == 'ffffffffffffffff'
	assert strconv.parse_int('baobab', 36, 64) == 683058467
	assert strconv.format_uint(683058467, 36) == 'baobab'
}
