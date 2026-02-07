// BEAM terminal implementation
module term

// get_terminal_size returns a number of columns and rows of terminal window.
// On BEAM: Uses io:columns() and io:rows()
pub fn get_terminal_size() (int, int) {
	// Placeholder - in real impl:
	// {ok, Cols} = io:columns() or default
	// {ok, Rows} = io:rows() or default
	return 80, 25
}

// clear clears current terminal screen using ANSI escape codes.
pub fn clear() bool {
	print('\x1b[2J')
	print('\x1b[H')
	return true
}

// input_character reads a single character from stdin
// On BEAM: Uses io:get_chars
fn input_character() int {
	// Placeholder - in real impl: io:get_chars('', 1)
	return -1
}

// supports_sixel returns true if the terminal supports SIXEL graphics.
// On BEAM: Would need to query terminal capabilities via escape sequences
pub fn supports_sixel() bool {
	// Most terminals in BEAM environments (like escript) don't support SIXEL
	// Real implementation would query $TERM and send DA1 escape sequence
	return false
}

// enable_echo enables or disables character echo on the terminal.
// On BEAM: No-op as terminal control is typically handled differently
pub fn enable_echo(enable bool) {
	// No-op for BEAM - terminal echo is managed by the runtime
}

// graphics_num_colors returns the number of colors supported by the terminal.
// On BEAM: Returns 0 as BEAM doesn't typically support terminal graphics
pub fn graphics_num_colors() u16 {
	return 0
}

// KeyPressedParams contains the optional parameters for key_pressed.
@[params]
pub struct KeyPressedParams {
pub mut:
	blocking bool // whether to wait for a pressed key
	echo     bool // whether to output the pressed key to stdout
}

// key_pressed returns the currently pressed key, or -1 if no key is pressed.
// On BEAM: Non-blocking keyboard input is not typically available
pub fn key_pressed(params KeyPressedParams) i64 {
	return -1
}
