import math.big

const named_integer_call_arg = 7

fn take_u32(value u32) u32 {
	return value
}

fn test_integer_constant_call_arguments_adopt_parameter_type() {
	assert take_u32(named_integer_call_arg) == 7
	assert take_u32(72 + 80 * 2) == 232
	assert take_u32(big.digit_bits * 10) == 600
}
