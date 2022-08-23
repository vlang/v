module os

#include <windows.h>

// input_password prompts the user for a password-like secret. It disables
// the terminal echo during user input and resets it back to normal when done.
pub fn input_password(prompt string) !string {
	if is_atty(1) <= 0 || getenv('TERM') == 'dumb' {
		return error('Could not obtain password discretely.')
	}

	std_handle := C.GetStdHandle(C.STD_INPUT_HANDLE)
	mut mode := u32(0)

	unsafe { C.GetConsoleMode(std_handle, &mode) }
	unsafe { C.SetConsoleMode(std_handle, mode & (~u32(C.ENABLE_ECHO_INPUT))) }

	defer {
		unsafe { C.SetConsoleMode(std_handle, &mode) }
		println('')
	}

	password := input_opt(prompt) or { return error('Failed to read password') }

	return password
}
