module term

import os

struct C.SMALL_RECT {
	Left   i16
	Top    i16
	Right  i16
	Bottom i16
}

struct C.CONSOLE_SCREEN_BUFFER_INFO {
	dwSize              C.COORD
	dwCursorPosition    C.COORD
	wAttributes         C.COORD
	srWindow            C.SMALL_RECT
	dwMaximumWindowSize C.COORD
}

fn C.GetConsoleScreenBufferInfo(handle C.HANDLE, info &CONSOLE_SCREEN_BUFFER_INFO) bool

// get_terminal_size returns a number of colums and rows of terminal window.
pub fn get_terminal_size() (int, int) {
	if is_atty(1) > 0 && os.getenv('TERM') != 'dumb' {
		info := CONSOLE_SCREEN_BUFFER_INFO{}

		if C.GetConsoleScreenBufferInfo(C.GetStdHandle(C.STD_OUTPUT_HANDLE), &info) {
			columns := (info.srWindow.Right - info.srWindow.Left + 1) as int
			rows := (info.srWindow.Bottom - info.srWindow.Top + 1) as int
			return columns, rows
		}
	}

	return default_columns_size, default_rows_size
}
