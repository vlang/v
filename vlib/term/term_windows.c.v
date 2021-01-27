module term

import os

[typedef]
struct C.COORD {
	X i16
	Y i16
}

[typedef]
struct C.SMALL_RECT {
	Left   u16
	Top    u16
	Right  u16
	Bottom u16
}

// win: CONSOLE_SCREEN_BUFFER_INFO
// https://docs.microsoft.com/en-us/windows/console/console-screen-buffer-info-str
[typedef]
struct C.CONSOLE_SCREEN_BUFFER_INFO {
	dwSize              C.COORD
	dwCursorPosition    C.COORD
	wAttributes         u16
	srWindow            C.SMALL_RECT
	dwMaximumWindowSize C.COORD
}

// ref - https://docs.microsoft.com/en-us/windows/console/getconsolescreenbufferinfo
fn C.GetConsoleScreenBufferInfo(handle os.HANDLE, info &C.CONSOLE_SCREEN_BUFFER_INFO) bool

// ref - https://docs.microsoft.com/en-us/windows/console/setconsoletitle
fn C.SetConsoleTitle(title &u16) bool

// get_terminal_size returns a number of colums and rows of terminal window.
pub fn get_terminal_size() (int, int) {
	if is_atty(1) > 0 && os.getenv('TERM') != 'dumb' {
		info := C.CONSOLE_SCREEN_BUFFER_INFO{}
		if C.GetConsoleScreenBufferInfo(C.GetStdHandle(C.STD_OUTPUT_HANDLE), &info) {
			columns := int(info.srWindow.Right - info.srWindow.Left + 1)
			rows := int(info.srWindow.Bottom - info.srWindow.Top + 1)
			return columns, rows
		}
	}
	return default_columns_size, default_rows_size
}

// get_cursor_position returns a Coord containing the current cursor position
pub fn get_cursor_position() Coord {
	mut res := Coord{}
	if is_atty(1) > 0 && os.getenv('TERM') != 'dumb' {
		info := C.CONSOLE_SCREEN_BUFFER_INFO{}
		if C.GetConsoleScreenBufferInfo(C.GetStdHandle(C.STD_OUTPUT_HANDLE), &info) {
			res.x = info.dwCursorPosition.X
			res.y = info.dwCursorPosition.Y
		}
	}
	return res
}

// set_terminal_title change the terminal title
pub fn set_terminal_title(title string) bool {
	title_change := C.SetConsoleTitle(title.to_wide())
	return title_change
}
