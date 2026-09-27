module main

fn test_main() {
	s := 'hello'
	arr := s.runes()
	assert arr.len == 5
}

fn rune_arg(value rune) rune {
	return value
}

fn byte_arg(value u8) u8 {
	return value
}

fn test_byte_and_rune_arguments_are_interchangeable() {
	byte := u8(`a`)
	assert rune_arg(byte) == `a`
	rune_value := if byte == `a` { `b` } else { `c` }
	assert byte_arg(rune_value) == u8(`b`)
}
