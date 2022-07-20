module term

import os

[typedef]
pub struct C.COORD {
mut:
	X i16
	Y i16
}

[typedef]
pub struct C.SMALL_RECT {
mut:
	Left   u16
	Top    u16
	Right  u16
	Bottom u16
}

// win: CONSOLE_SCREEN_BUFFER_INFO
// https://docs.microsoft.com/en-us/windows/console/console-screen-buffer-info-str
[typedef]
pub struct C.CONSOLE_SCREEN_BUFFER_INFO {
mut:
	dwSize              C.COORD
	dwCursorPosition    C.COORD
	wAttributes         u16
	srWindow            C.SMALL_RECT
	dwMaximumWindowSize C.COORD
}

pub union C.uChar {
mut:
	UnicodeChar rune
	AsciiChar   u8
}

[typedef]
pub struct C.CHAR_INFO {
mut:
	Char       C.uChar
	Attributes u16
}

// ref - https://docs.microsoft.com/en-us/windows/console/getconsolescreenbufferinfo
fn C.GetConsoleScreenBufferInfo(handle C.HANDLE, info &C.CONSOLE_SCREEN_BUFFER_INFO) bool

// ref - https://docs.microsoft.com/en-us/windows/console/setconsoletitle
fn C.SetConsoleTitle(title &u16) bool

// ref - https://docs.microsoft.com/en-us/windows/console/setconsolecursorposition
fn C.SetConsoleCursorPosition(handle C.HANDLE, coord C.COORD) bool

// ref - https://docs.microsoft.com/en-us/windows/console/scrollconsolescreenbuffer
fn C.ScrollConsoleScreenBuffer(output C.HANDLE, scroll_rect &C.SMALL_RECT, clip_rect &C.SMALL_RECT, des C.COORD, fill &C.CHAR_INFO) bool

// get_terminal_size returns a number of colums and rows of terminal window.
pub fn get_terminal_size() (int, int) {
	if os.is_atty(1) > 0 && os.getenv('TERM') != 'dumb' {
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
pub fn get_cursor_position() ?Coord {
	mut res := Coord{}
	if os.is_atty(1) > 0 && os.getenv('TERM') != 'dumb' {
		info := C.CONSOLE_SCREEN_BUFFER_INFO{}
		if C.GetConsoleScreenBufferInfo(C.GetStdHandle(C.STD_OUTPUT_HANDLE), &info) {
			res.x = info.dwCursorPosition.X
			res.y = info.dwCursorPosition.Y
		} else {
			return os.last_error()
		}
	}
	return res
}

// set_terminal_title change the terminal title
pub fn set_terminal_title(title string) bool {
	title_change := C.SetConsoleTitle(title.to_wide())
	return title_change
}

// clear clears current terminal screen.
// Implementation taken from https://docs.microsoft.com/en-us/windows/console/clearing-the-screen#example-2.
pub fn clear() {
	hconsole := C.GetStdHandle(C.STD_OUTPUT_HANDLE)
	mut csbi := C.CONSOLE_SCREEN_BUFFER_INFO{}
	mut scrollrect := C.SMALL_RECT{}
	mut scrolltarget := C.COORD{}
	mut fill := C.CHAR_INFO{}

	// Get the number of character cells in the current buffer.
	if !C.GetConsoleScreenBufferInfo(hconsole, &csbi) {
		return
	}
	// Scroll the rectangle of the entire buffer.
	scrollrect.Left = 0
	scrollrect.Top = 0
	scrollrect.Right = u16(csbi.dwSize.X)
	scrollrect.Bottom = u16(csbi.dwSize.Y)

	// Scroll it upwards off the top of the buffer with a magnitude of the entire height.
	scrolltarget.X = 0
	scrolltarget.Y = (0 - csbi.dwSize.Y)

	// Fill with empty spaces with the buffer's default text attribute.
	fill.Char.UnicodeChar = rune(` `)
	fill.Attributes = csbi.wAttributes

	// Do the scroll
	C.ScrollConsoleScreenBuffer(hconsole, &scrollrect, C.NULL, scrolltarget, &fill)

	// Move the cursor to the top left corner too.
	csbi.dwCursorPosition.X = 0
	csbi.dwCursorPosition.Y = 0

	C.SetConsoleCursorPosition(hconsole, csbi.dwCursorPosition)
}
