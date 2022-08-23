module os

#include <windows.h>

pub fn input_password(prompt string) !string {
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
