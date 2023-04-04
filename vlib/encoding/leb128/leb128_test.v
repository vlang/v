module leb128

fn test_encode_int() {
	assert encode_int(255) == [u8(255), 1]
	assert encode_int(-255) == [u8(129), 126]
	assert encode_int(127) == [u8(255), 0]
	assert encode_int(-127) == [u8(129), 127]
}

fn test_encode_u() {
	assert encode_u32(255) == [u8(255), 1]
	assert encode_u32(127) == [u8(127)]
	assert encode_u64(255) == [u8(255), 1]
	assert encode_u64(127) == [u8(127)]
}

fn test_decode_int() {
	assert decode_int([u8(255), 1]) == 255
	assert decode_int([u8(129), 126]) == -255
	assert decode_int([u8(255), 0]) == 127
	assert decode_int([u8(129), 127]) == -127
}

fn test_decode_u() {
	assert decode_u32([u8(255), 1]) == 255
	assert decode_u32([u8(127)]) == 127
	assert decode_u64([u8(255), 1]) == 255
	assert decode_u64([u8(127)]) == 127
}
