module term

import os

const (
	default_columns_size = 80
	default_rows_size = 25
)
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
	return if can_show_color_on_stdout() { green(s) } else { s }
}

// fail_message returns a colored string with red color.
// If colors are not allowed, returns a given string.
pub fn fail_message(s string) string {
	return if can_show_color_on_stdout() { bold(bg_red(white(s))) } else { s }
}

// warn_message returns a colored string with yellow color.
// If colors are not allowed, returns a given string.
pub fn warn_message(s string) string {
	return if can_show_color_on_stdout() { bright_yellow(s) } else { s }
}

// h_divider returns a horizontal divider line with a dynamic width,
// that depends on the current terminal settings.
// If an empty string is passed in, print enough spaces to make a new line
pub fn h_divider(divider string) string {
	cols,_ := get_terminal_size()
	result := if divider.len > 0 { divider.repeat(1 + (cols / divider.len)) } else { " ".repeat(1 + cols) }
	return result[0..cols]
}

// header returns a horizontal divider line with a centered text in the middle.
// e.g: term.header('TEXT', '=')
// =============== TEXT ===============
pub fn header(text, divider string) string {
	if text.len == 0 {
		return h_divider(divider)
	}
	cols,_ := get_terminal_size()
	tlimit := if cols > text.len + 2 + 2 * divider.len { text.len } else { cols - 3 - 2 * divider.len }
	tlimit_alligned := if (tlimit % 2) != (cols % 2) { tlimit + 1 } else { tlimit }
	tstart := (cols - tlimit_alligned) / 2
	ln := if divider.len > 0 { divider.repeat(1 + cols / divider.len)[0..cols] } else { " ".repeat(1 + cols) }
	return ln[0..tstart] + ' ' + text[0..tlimit] + ' ' + ln[tstart + tlimit + 2..cols]
}

fn supports_escape_sequences(fd int) bool {
	vcolors_override := os.getenv('VCOLORS')
	if vcolors_override == 'always' {
		return true
	}
	$if windows {
		return (is_atty(fd) & 0x0004) > 0 && os.getenv('TERM') != 'dumb' // enable_virtual_terminal_processing
	} $else {
		return is_atty(fd) > 0 && os.getenv('TERM') != 'dumb'
	}
}
