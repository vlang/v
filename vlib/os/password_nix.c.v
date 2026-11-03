module os

import term.termios

// input_password prompts the user for a password-like secret.
// It disables the terminal echo during user input and resets it back to normal when done.
pub fn input_password(prompt string) !string {
	if is_atty(1) <= 0 || getenv('TERM') == 'dumb' {
		return error('Could not obtain password discretely.')
	}

	mut old_state := termios.Termios{}
	if termios.tcgetattr(0, mut old_state) != 0 {
		return last_error()
	}

	mut new_state := old_state
	new_state.disable_echo()
	termios.set_state(0, new_state)
	// Restore the terminal on every path, including a failed read (e.g. Ctrl-D).
	defer {
		termios.set_state(0, old_state)
		println('')
	}

	password := input_opt(prompt) or { return error('Failed to read password') }
	return password
}
