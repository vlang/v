module os

import term.termios

// input_password prompts the user for a password-like secret. It disables
// the terminal echo during user input and resets it back to normal when done.
pub fn input_password(prompt string) !string {
	if is_atty(1) <= 0 || getenv('TERM') == 'dumb' {
		return error('Could not obtain password discretely.')
	}

	mut old_state := termios.Termios{}
	if termios.tcgetattr(0, mut old_state) != 0 {
		return last_error()
	}
	defer {
		termios.tcsetattr(0, C.TCSANOW, mut old_state)
		println('')
	}

	mut new_state := old_state

	new_state.c_lflag &= termios.invert(C.ECHO) // Disable echoing of characters
	termios.tcsetattr(0, C.TCSANOW, mut new_state)

	password := input_opt(prompt) or { return error('Failed to read password') }

	return password
}
