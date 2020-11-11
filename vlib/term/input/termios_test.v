module input

fn test_escape_end() {
	assert escape_end('\x1b[0m') == 4
	assert escape_end('\x1b[0m\x1b[1m') == 4
	assert escape_end('\x1b[OP\x1b[OQ') == 4
	assert escape_end('\x1bOS') == 3
	assert escape_end('\x1b[0;0m\x1b[1m') == 6
}
