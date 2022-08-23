module os

#include <termios.h>

pub struct C.termios {
mut:
	c_iflag int
	c_oflag int
	c_cflag int
	c_lflag int
	c_cc    [20]u8
}

fn C.tcgetattr(fd int, ptr &C.termios) int

fn C.tcsetattr(fd int, action int, const_ptr &C.termios)

// input_password prompts the user for a password-like secret. It disables
// the terminal echo during user input and resets it back to normal when done.
pub fn input_password(prompt string) !string {
	if is_atty(1) <= 0 || getenv('TERM') == 'dumb' {
		return error('Could not obtain password discretely.')
	}

	old_state := C.termios{}
	if unsafe { C.tcgetattr(0, &old_state) } != 0 {
		return last_error()
	}
	defer {
		unsafe { C.tcsetattr(0, C.TCSANOW, &old_state) }
		println('')
	}

	mut new_state := old_state

	new_state.c_lflag &= int(~u32(C.ECHO)) // Disable echoing of characters
	unsafe { C.tcsetattr(0, C.TCSANOW, &new_state) }

	password := input_opt(prompt) or { return error('Failed to read password') }

	return password
}
