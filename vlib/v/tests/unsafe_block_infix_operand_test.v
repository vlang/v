// An `unsafe` block used as an operand keeps the grouping of the infix inside it.
fn test_unsafe_block_infix_as_left_operand() {
	a := u32(1)
	b := u32(2)
	c := u32(3)
	assert unsafe { a | b } ^ c == 0
	assert (unsafe { a | b } ^ c) == ((a | b) ^ c)
}

fn test_unsafe_block_infix_as_right_operand() {
	n := 3
	assert unsafe { n } * unsafe { n + 1 } == 12
	assert 10 - unsafe { n - 1 } == 8
}

fn test_unsafe_block_infix_under_prefix() {
	n := 3
	assert -unsafe { n - 1 } == -2
}

fn test_unsafe_block_infix_with_lower_precedence_inside() {
	t := true
	n := 3
	assert !(unsafe { n > 2 } && unsafe { n < 2 || !t })
}

fn test_unsafe_block_shift_inside_xor() {
	p := [u8(0x01), 0x02, 0x03, 0x04]
	crc := u32(0xffffffff)
	lo := unsafe { u32(p[0]) | u32(p[1]) << 8 | u32(p[2]) << 16 | u32(p[3]) << 24 } ^ crc
	assert lo == u32(0x04030201) ^ crc
}
