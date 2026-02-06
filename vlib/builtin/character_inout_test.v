fn test_print_character() {
	assert print_character(u8(`a`)) > 0
	assert print_character(u8(`A`)) > 0
	println('')
	assert print_character(rune(`a`)) > 0
	assert print_character(rune(`A`)) > 0
	println('')
	assert print_character(u8(char(`a`))) > 0
	assert print_character(u8(char(`A`))) > 0
	println('')
	assert print_character(u8(int(`a`))) > 0
	assert print_character(u8(int(`A`))) > 0
	println('')
}

fn test_input_character() {
	// TODO: add an `expect` based test on Linux
}
