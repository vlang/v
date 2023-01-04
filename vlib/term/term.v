module term

import os
import strings.textscanner

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

// failed returns a bold white on red version of the string `s`
// If colors are not allowed, returns the string `s`
pub fn failed(s string) string {
	if can_show_color_on_stdout() {
		return bg_red(bold(white(s)))
	}
	return s
}

// ok_message returns a colored string with green color.
// If colors are not allowed, returns a given string.
pub fn ok_message(s string) string {
	if can_show_color_on_stdout() {
		return green(' ${s} ')
	}
	return s
}

// fail_message returns a colored string with red color.
// If colors are not allowed, returns a given string.
pub fn fail_message(s string) string {
	return failed(' ${s} ')
}

// warn_message returns a colored string with yellow color.
// If colors are not allowed, returns a given string.
pub fn warn_message(s string) string {
	if can_show_color_on_stdout() {
		return bright_yellow(' ${s} ')
	}
	return s
}

// colorize returns a colored string by running the specified `cfn` over
// the message `s`, only if colored stdout is supported by the terminal.
// Example: term.colorize(term.yellow, 'the message')
pub fn colorize(cfn fn (string) string, s string) string {
	if can_show_color_on_stdout() {
		return cfn(s)
	}
	return s
}

// ecolorize returns a colored string by running the specified `cfn` over
// the message `s`, only if colored stderr is supported by the terminal.
// Example: term.ecolorize(term.bright_red, 'the message')
pub fn ecolorize(cfn fn (string) string, s string) string {
	if can_show_color_on_stderr() {
		return cfn(s)
	}
	return s
}

// strip_ansi removes any ANSI sequences in the `text`
pub fn strip_ansi(text string) string {
	// This is a port of https://github.com/kilobyte/colorized-logs/blob/master/ansi2txt.c
	// \e, [, 1, m, a, b, c, \e, [, 2, 2, m => abc
	mut input := textscanner.new(text)
	mut output := []u8{cap: text.len}
	mut ch := 0
	for ch != -1 {
		ch = input.next()
		if ch == 27 {
			ch = input.next()
			if ch == `[` {
				for {
					ch = input.next()
					if ch in [`;`, `?`] || (ch >= `0` && ch <= `9`) {
						continue
					}
					break
				}
			} else if ch == `]` {
				ch = input.next()
				if ch >= `0` && ch <= `9` {
					for {
						ch = input.next()
						if ch == -1 || ch == 7 {
							break
						}
						if ch == 27 {
							ch = input.next()
							break
						}
					}
				}
			} else if ch == `%` {
				ch = input.next()
			}
		} else if ch != -1 {
			output << u8(ch)
		}
	}
	return output.bytestr()
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

// header_left returns a horizontal divider line with a title text on the left.
// e.g: term.header_left('TITLE', '=')
// ==== TITLE =========================
pub fn header_left(text string, divider string) string {
	plain_text := strip_ansi(text)
	xcols, _ := get_terminal_size() // can get 0 in lldb/gdb
	cols := imax(1, xcols)
	relement := if divider.len > 0 { divider } else { ' ' }
	hstart := relement.repeat(4)[0..4]
	remaining_cols := imax(0, (cols - (hstart.len + 1 + plain_text.len + 1)))
	hend := relement.repeat((remaining_cols + 1) / relement.len)[0..remaining_cols]
	return '${hstart} ${text} ${hend}'
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

[manualfree]
fn supports_escape_sequences(fd int) bool {
	vcolors_override := os.getenv('VCOLORS')
	defer {
		unsafe { vcolors_override.free() }
	}
	if vcolors_override == 'always' {
		return true
	}
	if vcolors_override == 'never' {
		return false
	}
	env_term := os.getenv('TERM')
	defer {
		unsafe { env_term.free() }
	}
	if env_term == 'dumb' {
		return false
	}
	$if windows {
		env_conemu := os.getenv('ConEmuANSI')
		defer {
			unsafe { env_conemu.free() }
		}
		if env_conemu == 'ON' {
			return true
		}
		// 4 is enable_virtual_terminal_processing
		return (os.is_atty(fd) & 0x0004) > 0
	} $else {
		return os.is_atty(fd) > 0
	}
}
