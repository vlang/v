module term

import os

const (
	default_columns_size = 80
	default_rows_size    = 25
)

// Coord - used by term.get_cursor_position and term.set_cursor_position
pub struct Coord {
pub mut:
	x int
	y int
}

// can_show_color_on_stdout returns true if colors are allowed in stdout;
// returns false otherwise.
pub fn can_show_color_on_stdout() bool {
	return supports_escape_sequences(1)
}

// can_show_color_on_stderr returns true if colors are allowed in stderr;
// returns false otherwise.
pub fn can_show_color_on_stderr() bool {
	return supports_escape_sequences(2)
}

// ok_message returns a colored string with green color.
// If colors are not allowed, returns a given string.
pub fn ok_message(s string) string {
	if can_show_color_on_stdout() {
		return green(' $s ')
	}
	return s
}

// fail_message returns a colored string with red color.
// If colors are not allowed, returns a given string.
pub fn fail_message(s string) string {
	if can_show_color_on_stdout() {
		return inverse(bg_white(bold(red(' $s '))))
	}
	return s
}

// warn_message returns a colored string with yellow color.
// If colors are not allowed, returns a given string.
pub fn warn_message(s string) string {
	if can_show_color_on_stdout() {
		return bright_yellow(' $s ')
	}
	return s
}

// colorize returns a colored string by running the specified `cfn` over
// the message `s`, only if colored output is supported by the terminal.
// Example: term.colorize(term.yellow, 'the message')
pub fn colorize(cfn fn (string) string, s string) string {
	if can_show_color_on_stdout() {
		return cfn(s)
	}
	return s
}

// h_divider returns a horizontal divider line with a dynamic width,
// that depends on the current terminal settings.
// If an empty string is passed in, print enough spaces to make a new line
pub fn h_divider(divider string) string {
	cols, _ := get_terminal_size()
	mut result := ''
	if divider.len > 0 {
		result = divider.repeat(1 + (cols / divider.len))
	} else {
		result = ' '.repeat(1 + cols)
	}
	return result[0..cols]
}

// header returns a horizontal divider line with a centered text in the middle.
// e.g: term.header('TEXT', '=')
// =============== TEXT ===============
pub fn header(text string, divider string) string {
	if text.len == 0 {
		return h_divider(divider)
	}
	xcols, _ := get_terminal_size()
	cols := imax(1, xcols)
	tlimit := imax(1, if cols > text.len + 2 + 2 * divider.len {
		text.len
	} else {
		cols - 3 - 2 * divider.len
	})
	tlimit_alligned := if (tlimit % 2) != (cols % 2) { tlimit + 1 } else { tlimit }
	tstart := imax(0, (cols - tlimit_alligned) / 2)
	mut ln := ''
	if divider.len > 0 {
		ln = divider.repeat(1 + cols / divider.len)[0..cols]
	} else {
		ln = ' '.repeat(1 + cols)
	}
	if ln.len == 1 {
		return ln + ' ' + text[0..tlimit] + ' ' + ln
	}
	return ln[0..tstart] + ' ' + text[0..tlimit] + ' ' + ln[tstart + tlimit + 2..cols]
}

fn imax(x int, y int) int {
	return if x > y { x } else { y }
}

fn supports_escape_sequences(fd int) bool {
	vcolors_override := os.getenv('VCOLORS')
	if vcolors_override == 'always' {
		return true
	}
	if vcolors_override == 'never' {
		return false
	}
	if os.getenv('TERM') == 'dumb' {
		return false
	}
	$if windows {
		if os.getenv('ConEmuANSI') == 'ON' {
			return true
		}
		// 4 is enable_virtual_terminal_processing
		return (is_atty(fd) & 0x0004) > 0
	} $else {
		return is_atty(fd) > 0
	}
}
