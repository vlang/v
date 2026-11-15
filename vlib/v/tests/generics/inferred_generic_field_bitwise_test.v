import math

struct BitwiseOperand[T] {
	value T
}

fn bitwise_operand[T](value T) BitwiseOperand[T] {
	return BitwiseOperand[T]{ value: value }
}

fn check_bitwise_result(actual int, expected int) {
	assert actual == expected
}

fn test_inferred_generic_fields_in_bitwise_expressions() {
	check_bitwise_result(bitwise_operand(3).value << 1, 6)
	check_bitwise_result(1 << bitwise_operand(3).value, 8)
	check_bitwise_result(bitwise_operand(12).value >> 1, 6)
	assert (bitwise_operand(i32(-2)).value >>> 1) == u32(2147483647)
	check_bitwise_result(bitwise_operand(7).value & 3, 3)
	check_bitwise_result(1 | bitwise_operand(4).value, 5)
	check_bitwise_result(bitwise_operand(7).value ^ 3, 4)
	shifted := bitwise_operand(4).value << 2
	assert shifted == 16
	mut total := 1
	total <<= bitwise_operand(3).value
	assert total == 8
	check_bitwise_result(math.divide_floored(7, 3).quot << 1, 4)
}
