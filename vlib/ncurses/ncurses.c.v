module ncurses

// Window is an opaque handle to an ncurses window.
pub type Window = voidptr

$if !windows {
	$if $pkgconfig('ncursesw') {
		#pkgconfig --cflags --libs ncursesw
	} $else $if $pkgconfig('ncurses') {
		#pkgconfig --cflags --libs ncurses
	} $else {
		#flag darwin -D_DARWIN_C_SOURCE -lncurses
		#flag linux -lncursesw
		#flag freebsd -lncursesw
		#flag openbsd -lcurses
		#flag netbsd -lcurses
		#flag dragonfly -lncurses
		#flag solaris -lncurses
	}
	#insert "@VEXEROOT/vlib/ncurses/ncurses_helpers.h"

	fn C.v_ncurses_initscr() Window
	fn C.v_ncurses_stdscr() Window
	fn C.v_ncurses_endwin() int
	fn C.v_ncurses_cbreak() int
	fn C.v_ncurses_nocbreak() int
	fn C.v_ncurses_raw() int
	fn C.v_ncurses_noraw() int
	fn C.v_ncurses_echo() int
	fn C.v_ncurses_noecho() int
	fn C.v_ncurses_keypad(win Window, enabled int) int
	fn C.v_ncurses_nodelay(win Window, enabled int) int
	fn C.v_ncurses_timeout(delay int)
	fn C.v_ncurses_wtimeout(win Window, delay int)
	fn C.v_ncurses_curs_set(visibility int) int
	fn C.v_ncurses_clear() int
	fn C.v_ncurses_refresh() int
	fn C.v_ncurses_getch() int
	fn C.v_ncurses_addstr(text &char) int
	fn C.v_ncurses_mvaddstr(y int, x int, text &char) int
	fn C.v_ncurses_newwin(lines int, cols int, begin_y int, begin_x int) Window
	fn C.v_ncurses_delwin(win Window) int
	fn C.v_ncurses_box(win Window, vertical u32, horizontal u32) int
	fn C.v_ncurses_getmaxx(win Window) int
	fn C.v_ncurses_getmaxy(win Window) int
	fn C.v_ncurses_wrefresh(win Window) int
	fn C.v_ncurses_wclear(win Window) int
	fn C.v_ncurses_wgetch(win Window) int
	fn C.v_ncurses_wmove(win Window, y int, x int) int
	fn C.v_ncurses_waddstr(win Window, text &char) int
	fn C.v_ncurses_mvwaddstr(win Window, y int, x int, text &char) int
	fn C.v_ncurses_start_color() int
	fn C.v_ncurses_has_colors() int
	fn C.v_ncurses_init_pair(pair i16, fg i16, bg i16) int
	fn C.v_ncurses_color_pair(pair int) int
	fn C.v_ncurses_attron(attr int) int
	fn C.v_ncurses_attroff(attr int) int
	fn C.v_ncurses_wattron(win Window, attr int) int
	fn C.v_ncurses_wattroff(win Window, attr int) int
	fn C.v_ncurses_key_f(n int) int
}

$if windows {
	pub const ok = 0
	pub const err = -1
	pub const key_code_yes = 0
	pub const key_down = 0
	pub const key_up = 0
	pub const key_left = 0
	pub const key_right = 0
	pub const key_home = 0
	pub const key_end = 0
	pub const key_npage = 0
	pub const key_ppage = 0
	pub const key_ic = 0
	pub const key_dc = 0
	pub const key_backspace = 0
	pub const key_enter = 0
	pub const cursor_invisible = 0
	pub const cursor_normal = 1
	pub const cursor_visible = 2
	pub const color_black = 0
	pub const color_red = 1
	pub const color_green = 2
	pub const color_yellow = 3
	pub const color_blue = 4
	pub const color_magenta = 5
	pub const color_cyan = 6
	pub const color_white = 7
	pub const a_normal = 0
	pub const a_standout = 0
	pub const a_underline = 0
	pub const a_reverse = 0
	pub const a_blink = 0
	pub const a_dim = 0
	pub const a_bold = 0
} $else {
	pub const ok = int(C.OK)
	pub const err = int(C.ERR)
	pub const key_code_yes = int(C.KEY_CODE_YES)
	pub const key_down = int(C.KEY_DOWN)
	pub const key_up = int(C.KEY_UP)
	pub const key_left = int(C.KEY_LEFT)
	pub const key_right = int(C.KEY_RIGHT)
	pub const key_home = int(C.KEY_HOME)
	pub const key_end = int(C.KEY_END)
	pub const key_npage = int(C.KEY_NPAGE)
	pub const key_ppage = int(C.KEY_PPAGE)
	pub const key_ic = int(C.KEY_IC)
	pub const key_dc = int(C.KEY_DC)
	pub const key_backspace = int(C.KEY_BACKSPACE)
	pub const key_enter = int(C.KEY_ENTER)
	pub const cursor_invisible = 0
	pub const cursor_normal = 1
	pub const cursor_visible = 2
	pub const color_black = int(C.COLOR_BLACK)
	pub const color_red = int(C.COLOR_RED)
	pub const color_green = int(C.COLOR_GREEN)
	pub const color_yellow = int(C.COLOR_YELLOW)
	pub const color_blue = int(C.COLOR_BLUE)
	pub const color_magenta = int(C.COLOR_MAGENTA)
	pub const color_cyan = int(C.COLOR_CYAN)
	pub const color_white = int(C.COLOR_WHITE)
	pub const a_normal = int(C.A_NORMAL)
	pub const a_standout = int(C.A_STANDOUT)
	pub const a_underline = int(C.A_UNDERLINE)
	pub const a_reverse = int(C.A_REVERSE)
	pub const a_blink = int(C.A_BLINK)
	pub const a_dim = int(C.A_DIM)
	pub const a_bold = int(C.A_BOLD)
}

fn unsupported_panic() {
	panic('ncurses is not supported on ${@OS}')
}

@[inline]
fn bool_to_c(enabled bool) int {
	return if enabled { 1 } else { 0 }
}

// is_supported reports whether this target has an ncurses backend in the standard library.
pub fn is_supported() bool {
	$if windows {
		return false
	} $else {
		return true
	}
}

// initscr initializes the curses screen and returns the default window handle.
pub fn initscr() Window {
	$if windows {
		unsupported_panic()
		return unsafe { nil }
	} $else {
		return C.v_ncurses_initscr()
	}
}

// stdscr returns the current default screen window handle.
pub fn stdscr() Window {
	$if windows {
		unsupported_panic()
		return unsafe { nil }
	} $else {
		return C.v_ncurses_stdscr()
	}
}

// endwin restores the terminal after `initscr`.
pub fn endwin() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_endwin()
	}
}

// cbreak disables line buffering while keeping signal keys active.
pub fn cbreak() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_cbreak()
	}
}

// nocbreak restores normal line buffering after `cbreak`.
pub fn nocbreak() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_nocbreak()
	}
}

// raw enables raw input mode.
pub fn raw() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_raw()
	}
}

// noraw disables raw input mode.
pub fn noraw() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_noraw()
	}
}

// echo enables terminal echo for characters typed by the user.
pub fn echo() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_echo()
	}
}

// noecho disables terminal echo for characters typed by the user.
pub fn noecho() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_noecho()
	}
}

// keypad enables or disables keypad processing for a window.
pub fn keypad(win Window, enabled bool) int {
	$if windows {
		_ = win
		_ = enabled
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_keypad(win, bool_to_c(enabled))
	}
}

// nodelay enables or disables nonblocking reads for a window.
pub fn nodelay(win Window, enabled bool) int {
	$if windows {
		_ = win
		_ = enabled
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_nodelay(win, bool_to_c(enabled))
	}
}

// timeout sets the blocking behavior for `getch` on the default screen.
pub fn timeout(delay int) {
	$if windows {
		_ = delay
		unsupported_panic()
	} $else {
		C.v_ncurses_timeout(delay)
	}
}

// wtimeout sets the blocking behavior for `wgetch` on a specific window.
pub fn wtimeout(win Window, delay int) {
	$if windows {
		_ = win
		_ = delay
		unsupported_panic()
	} $else {
		C.v_ncurses_wtimeout(win, delay)
	}
}

// curs_set changes cursor visibility using the module cursor constants.
pub fn curs_set(visibility int) int {
	$if windows {
		_ = visibility
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_curs_set(visibility)
	}
}

// clear clears the default screen buffer.
pub fn clear() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_clear()
	}
}

// refresh flushes pending changes for the default screen.
pub fn refresh() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_refresh()
	}
}

// getch reads one key code from the default screen.
pub fn getch() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_getch()
	}
}

// addstr writes a string to the default screen at the current cursor position.
pub fn addstr(text string) int {
	$if windows {
		_ = text
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_addstr(&char(text.str))
	}
}

// mvaddstr moves the cursor on the default screen and writes a string there.
pub fn mvaddstr(y int, x int, text string) int {
	$if windows {
		_ = y
		_ = x
		_ = text
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_mvaddstr(y, x, &char(text.str))
	}
}

// newwin creates a new curses window.
pub fn newwin(lines int, cols int, begin_y int, begin_x int) Window {
	$if windows {
		_ = lines
		_ = cols
		_ = begin_y
		_ = begin_x
		unsupported_panic()
		return unsafe { nil }
	} $else {
		return C.v_ncurses_newwin(lines, cols, begin_y, begin_x)
	}
}

// delwin destroys a window created by `newwin`.
pub fn delwin(win Window) int {
	$if windows {
		_ = win
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_delwin(win)
	}
}

// box draws a border around a window. Pass `0` for the default border characters.
pub fn box(win Window, vertical int, horizontal int) int {
	$if windows {
		_ = win
		_ = vertical
		_ = horizontal
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_box(win, u32(vertical), u32(horizontal))
	}
}

// getmaxx returns the width of a window in columns.
pub fn getmaxx(win Window) int {
	$if windows {
		_ = win
		unsupported_panic()
		return 0
	} $else {
		return C.v_ncurses_getmaxx(win)
	}
}

// getmaxy returns the height of a window in rows.
pub fn getmaxy(win Window) int {
	$if windows {
		_ = win
		unsupported_panic()
		return 0
	} $else {
		return C.v_ncurses_getmaxy(win)
	}
}

// wrefresh flushes pending changes for a specific window.
pub fn wrefresh(win Window) int {
	$if windows {
		_ = win
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_wrefresh(win)
	}
}

// wclear clears a specific window.
pub fn wclear(win Window) int {
	$if windows {
		_ = win
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_wclear(win)
	}
}

// wgetch reads one key code from a specific window.
pub fn wgetch(win Window) int {
	$if windows {
		_ = win
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_wgetch(win)
	}
}

// wmove moves the cursor inside a specific window.
pub fn wmove(win Window, y int, x int) int {
	$if windows {
		_ = win
		_ = y
		_ = x
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_wmove(win, y, x)
	}
}

// waddstr writes a string to a specific window at its current cursor position.
pub fn waddstr(win Window, text string) int {
	$if windows {
		_ = win
		_ = text
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_waddstr(win, &char(text.str))
	}
}

// mvwaddstr moves the cursor in a window and writes a string there.
pub fn mvwaddstr(win Window, y int, x int, text string) int {
	$if windows {
		_ = win
		_ = y
		_ = x
		_ = text
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_mvwaddstr(win, y, x, &char(text.str))
	}
}

// start_color enables ncurses color support.
pub fn start_color() int {
	$if windows {
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_start_color()
	}
}

// has_colors reports whether the terminal supports colors.
pub fn has_colors() bool {
	$if windows {
		return false
	} $else {
		return C.v_ncurses_has_colors() != 0
	}
}

// init_pair defines a foreground/background color pair for use with `color_pair`.
pub fn init_pair(pair int, fg int, bg int) int {
	$if windows {
		_ = pair
		_ = fg
		_ = bg
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_init_pair(i16(pair), i16(fg), i16(bg))
	}
}

// color_pair returns the attribute mask for a color pair number.
pub fn color_pair(pair int) int {
	$if windows {
		_ = pair
		unsupported_panic()
		return 0
	} $else {
		return C.v_ncurses_color_pair(pair)
	}
}

// attron enables an attribute on the default screen.
pub fn attron(attr int) int {
	$if windows {
		_ = attr
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_attron(attr)
	}
}

// attroff disables an attribute on the default screen.
pub fn attroff(attr int) int {
	$if windows {
		_ = attr
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_attroff(attr)
	}
}

// wattron enables an attribute on a specific window.
pub fn wattron(win Window, attr int) int {
	$if windows {
		_ = win
		_ = attr
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_wattron(win, attr)
	}
}

// wattroff disables an attribute on a specific window.
pub fn wattroff(win Window, attr int) int {
	$if windows {
		_ = win
		_ = attr
		unsupported_panic()
		return err
	} $else {
		return C.v_ncurses_wattroff(win, attr)
	}
}

// key_f returns the key code for function keys like F1, F2, and so on.
pub fn key_f(n int) int {
	$if windows {
		_ = n
		unsupported_panic()
		return 0
	} $else {
		return C.v_ncurses_key_f(n)
	}
}
