module term_input

fn test_escape_end() {
	assert escape_end('\x1b[0m') == 4
	assert escape_end('\x1b[0m\x1b[1m') == 4
	assert escape_end('\x1b[0;0m\x1b[1m') == 6
}
