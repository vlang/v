module term

import os
import time

#include <conio.h>

@[typedef]
pub struct C.COORD {
mut:
	X i16
	Y i16
}

@[typedef]
pub struct C.SMALL_RECT {
mut:
	Left   u16
	Top    u16
	Right  u16
	Bottom u16
}

// win: CONSOLE_SCREEN_BUFFER_INFO
// https://docs.microsoft.com/en-us/windows/console/console-screen-buffer-info-str
@[typedef]
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

@[typedef]
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
fn C.ScrollConsoleScreenBuffer(output C.HANDLE, scroll_rect &C.SMALL_RECT, clip_rect &C.SMALL_RECT, des C.COORD,
	fill &C.CHAR_INFO) bool

// get_terminal_size returns a number of columns and rows of terminal window.
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

// get_cursor_position returns a Coord containing the current cursor position.
pub fn get_cursor_position() !Coord {
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

// set_terminal_title changes the terminal title.
pub fn set_terminal_title(title string) bool {
	wide_title := title.to_wide()
	return C.SetConsoleTitle(wide_title)
}

// set_tab_title changes the terminal *tab title*, for terminal emulators that do support several tabs.
pub fn set_tab_title(title string) bool {
	// TODO: investigate, whether there is an API for changing just the tab title on windows yet.
	return set_terminal_title(title)
}

// clear clears current terminal screen.
// Implementation taken from https://docs.microsoft.com/en-us/windows/console/clearing-the-screen#example-2.
pub fn clear() bool {
	hconsole := C.GetStdHandle(C.STD_OUTPUT_HANDLE)
	mut csbi := C.CONSOLE_SCREEN_BUFFER_INFO{}
	mut scrollrect := C.SMALL_RECT{}
	mut scrolltarget := C.COORD{}
	mut fill := C.CHAR_INFO{}

	// Get the number of character cells in the current buffer.
	if !C.GetConsoleScreenBufferInfo(hconsole, &csbi) {
		return false
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
	return true
}

// supports_sixel returns `true` if the terminal supports Sixel graphics
//
// For more info on the sixel format:
// See https://en.wikipedia.org/wiki/Sixel
// See https://www.digiater.nl/openvms/decus/vax90b1/krypton-nasa/all-about-sixels.text
// For more info on terminal support:
// See https://www.arewesixelyet.com
pub fn supports_sixel() bool {
	// According to (2024) https://www.arewesixelyet.com/#windows-console there's no support
	return false
}

// graphics_num_colors returns the number of color registers the terminal
// graphic attribute is set to use. This can be useful to know if the terminal
// is configured to support Sixel graphics.
//
// See "CSI ? Pi ; Pa ; Pv S" from https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
pub fn graphics_num_colors() u16 {
	// Since this call is related to sixel terminal graphics and Windows Console and Terminal
	// does not have support for querying the graphics setup this call returns 0
	return 0
}

// enable_echo enable/disable echo input characters.
pub fn enable_echo(enable bool) {
	// no need under windows, use key_pressed func's echo
}

fn C.kbhit() bool
fn C._getch() int
fn C._getche() int

// KeyPressedParams contains the optional parameters that you can pass to key_pressed.
@[params]
pub struct KeyPressedParams {
pub mut:
	blocking bool // whether to wait for a pressed key
	echo     bool // whether to output the pressed key to stdout
}

// key_pressed gives back a single character, read from the standard input.
// It returns -1 on error or no character in non-blocking mode
pub fn key_pressed(params KeyPressedParams) i64 {
	for {
		if C.kbhit() {
			res := if params.echo {
				C._getche()
			} else {
				C._getch()
			}
			// see https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/getche-getwche?view=msvc-170
			// > When _getche or _getwche reads a function key or an arrow key, the function must be called twice;
			// > the first call returns 0 or 0xE0, and the second call returns the actual key code.
			if res in [0, 0xe0] {
				if C.kbhit() {
					res2 := if params.echo {
						C._getche()
					} else {
						C._getch()
					}
					return i64(u32(0xe0) << 16 | u32(res2))
				}
			}
			return i64(res)
		}
		if !params.blocking {
			// in non-blocking mode, we need to return immediately
			return -1
		}
		time.sleep(1 * time.millisecond)
	}
	return 0
}
