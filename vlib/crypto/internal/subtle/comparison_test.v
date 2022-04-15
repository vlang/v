module subtle

fn test_constant_time_byte_eq() {
	assert constant_time_byte_eq(0, 0) == 1
	assert constant_time_byte_eq(1, 1) == 1
	assert constant_time_byte_eq(255, 255) == 1
	assert constant_time_byte_eq(255, 1) == 0
	assert constant_time_byte_eq(1, 255) == 0
	assert constant_time_byte_eq(2, 1) == 0
}

fn test_constant_time_eq() {
	assert constant_time_eq(0, 0) == 1
	assert constant_time_eq(255, 255) == 1
	assert constant_time_eq(65536, 65536) == 1
	assert constant_time_eq(-1, -1) == 1
	assert constant_time_eq(-256, -256) == 1
	assert constant_time_eq(0, 1) == 0
}

fn test_constant_time_select() {
	assert constant_time_select(1, 1, 0) == 1
	assert constant_time_select(1, 1, 255) == 1
	assert constant_time_select(1, 1, 255 * 255) == 1
	assert constant_time_select(1, 2, 0) == 2
	assert constant_time_select(1, 2, 255) == 2
	assert constant_time_select(1, 2, 255 * 255) == 2
	//
	assert constant_time_select(0, 1, 0) == 0
	assert constant_time_select(0, 1, 255) == 255
	assert constant_time_select(0, 1, 255 * 255) == 255 * 255
	assert constant_time_select(0, 2, 0) == 0
	assert constant_time_select(0, 2, 255) == 255
	assert constant_time_select(0, 2, 255 * 255) == 255 * 255
}

fn test_constant_time_compare() {
	assert constant_time_compare([u8(1), 2, 3], [u8(1), 2, 3]) == 1
	assert constant_time_compare([u8(1), 2, 3], [u8(1), 2, 9]) == 0
	assert constant_time_compare([u8(1), 2, 3], [u8(1), 2, 3, 4]) == 0
	assert constant_time_compare([u8(1), 2, 3], [u8(1), 2]) == 0
}

fn test_constant_time_copy() {
	y := [u8(3), 4, 5]
	mut x := [u8(0), 0, 0]
	constant_time_copy(0, mut x, y)
	assert x == [u8(0), 0, 0]
	constant_time_copy(1, mut x, y)
	assert x == y
	assert x == [u8(3), 4, 5]
}

fn test_constant_time_less_or_eq() {
	assert constant_time_less_or_eq(1, 1) == 1
	assert constant_time_less_or_eq(1, 2) == 1
	assert constant_time_less_or_eq(1, 3) == 1
	assert constant_time_less_or_eq(255, 255) == 1
	assert constant_time_less_or_eq(255, 256) == 1
	assert constant_time_less_or_eq(255, 257) == 1
	assert constant_time_less_or_eq(1, 0) == 0
	assert constant_time_less_or_eq(2, 1) == 0
	assert constant_time_less_or_eq(3, 2) == 0
	assert constant_time_less_or_eq(255, 3) == 0
}
