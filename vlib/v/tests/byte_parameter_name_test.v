fn utf8_second_context(byte u8) int {
	return int(byte)
}

fn byte_after_another_parameter(prefix int, byte u8) int {
	return prefix + int(byte)
}

fn byte_in_grouped_parameters(byte, other u8) int {
	return int(byte) + int(other)
}

fn byte_after_grouped_parameter(other, byte u8) int {
	return int(other) + int(byte)
}

fn increment_byte_parameter(mut byte []u8) {
	byte[0]++
}

fn test_byte_as_parameter_name() {
	assert utf8_second_context(0) == 0
	assert utf8_second_context(0x80) == 128
	assert utf8_second_context(0xff) == 255
}

fn test_byte_as_later_parameter_name() {
	assert byte_after_another_parameter(10, 32) == 42
}

fn test_byte_as_grouped_parameter_name() {
	assert byte_in_grouped_parameters(20, 22) == 42
	assert byte_after_grouped_parameter(20, 22) == 42
}

fn test_byte_as_mutable_parameter_name() {
	mut values := [u8(41)]
	increment_byte_parameter(mut values)
	assert values == [u8(42)]
}

fn test_byte_as_function_literal_parameter_name() {
	identity := fn (byte u8) int {
		return int(byte)
	}
	assert identity(42) == 42
}
