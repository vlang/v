// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module term

import strings

// format_esc produces an ANSI escape code, for selecting the graphics rendition of the following text.
// Each of the attributes that can be passed in `code`, separated by `;`, will be in effect,
// until the terminal encounters another SGR ANSI escape code. For more details about the different
// codes, and their meaning, see: https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters
pub fn format_esc(code string) string {
	return '\x1b[${code}m'
}

// format returns ANSI escape coded `msg` formatted with a preceding `open` and a succeeding `close`.
// For instance, `format('hi', '9', '29')` returns `'\x1b[9mhi\x1b[29m'`,
// or 'hi' with strikethrough, where `'\x1b[9m'` represents
// crossed out/strikethrough text and `'\x1b[29m'` turns off strikethrough.
pub fn format(msg string, open string, close string) string {
	return '\x1b[${open}m${msg}\x1b[${close}m'
}

// format_rgb returns ANSI escape coded `msg` formatted with a preceding `open`, a succeeding `close` and the provided RGB colors `r`, `g`, and `b`.
pub fn format_rgb(r int, g int, b int, msg string, open string, close string) string {
	return '\x1b[${open};2;${r};${g};${b}m${msg}\x1b[${close}m'
}

// rbg returns the `msg` with the foreground in the specified RGB color
// For example, `rgb(0, 255, 0, 'hi')` returns the `'hi'` string in
// lime color.
pub fn rgb(r int, g int, b int, msg string) string {
	return format_rgb(r, g, b, msg, '38', '39')
}

// bg_rgb returns the `msg` with the background in the specified RGB color.
// For example, `bg_rgb(255, 0, 0, 'hi')` returns the text `'hi'` in
// red color.
pub fn bg_rgb(r int, g int, b int, msg string) string {
	return format_rgb(r, g, b, msg, '48', '49')
}

// hex returns the `msg` with the foreground in the specified `hex` color.
// For example, `rgb(255, 'hi')` returns the `'hi'` string in
// blue color, which is `(0, 0, 255)` in RGB.
pub fn hex(hex int, msg string) string {
	return format_rgb(hex >> 16, (hex >> 8) & 0xFF, hex & 0xFF, msg, '38', '39')
}

// hex returns the `msg` with the background in the specified `hex` color.
// For example, `bg_rgb(255, 'hi')` returns the `'hi'` string in
// a background of blue color, which is `(0, 0, 255)` in RGB.
pub fn bg_hex(hex int, msg string) string {
	return format_rgb(hex >> 16, (hex >> 8) & 0xFF, hex & 0xFF, msg, '48', '49')
}

// reset resets all formatting for `msg`.
pub fn reset(msg string) string {
	return format(msg, '0', '0')
}

// bold returns the given `msg` in bold.
pub fn bold(msg string) string {
	return format(msg, '1', '22')
}

// dim returns the dimmed `msg`.
pub fn dim(msg string) string {
	return format(msg, '2', '22')
}

// italic returns the given `msg` in italic.
pub fn italic(msg string) string {
	return format(msg, '3', '23')
}

// underline returns the underlined `msg`.
pub fn underline(msg string) string {
	return format(msg, '4', '24')
}

// slow_blink will surround the `msg` with ANSI escape codes for blinking (less than 150 times per minute).
pub fn slow_blink(msg string) string {
	return format(msg, '5', '25')
}

// rapid_blink will surround the `msg` with ANSI escape codes for blinking (over 150 times per minute).
// Note that unlike slow_blink, this is not very widely supported.
pub fn rapid_blink(msg string) string {
	return format(msg, '6', '26')
}

// inverse inverses the given `msg`.
pub fn inverse(msg string) string {
	return format(msg, '7', '27')
}

// hidden hides the given `msg`.
pub fn hidden(msg string) string {
	return format(msg, '8', '28')
}

// strikethrough returns the given `msg` in strikethrough.
pub fn strikethrough(msg string) string {
	return format(msg, '9', '29')
}

// black formats the `msg` in black.
pub fn black(msg string) string {
	return format(msg, '30', '39')
}

// red formats the `msg` in red.
pub fn red(msg string) string {
	return format(msg, '31', '39')
}

// green formats the `msg` in green.
pub fn green(msg string) string {
	return format(msg, '32', '39')
}

// yellow formats the `msg` in yellow.
pub fn yellow(msg string) string {
	return format(msg, '33', '39')
}

// blue formats the `msg` in blue.
pub fn blue(msg string) string {
	return format(msg, '34', '39')
}

// magenta formats the `msg` in magenta.
pub fn magenta(msg string) string {
	return format(msg, '35', '39')
}

// cyan formats the `msg` in cyan.
pub fn cyan(msg string) string {
	return format(msg, '36', '39')
}

// white formats the `msg` in white.
pub fn white(msg string) string {
	return format(msg, '37', '39')
}

// bg_black formats the `msg` in black background.
pub fn bg_black(msg string) string {
	return format(msg, '40', '49')
}

// bg_red formats the `msg` in red background.
pub fn bg_red(msg string) string {
	return format(msg, '41', '49')
}

// bg_green formats the `msg` in green background.
pub fn bg_green(msg string) string {
	return format(msg, '42', '49')
}

// bg_yellow formats the `msg` in yellow background.
pub fn bg_yellow(msg string) string {
	return format(msg, '43', '49')
}

// bg_blue formats the `msg` in blue background.
pub fn bg_blue(msg string) string {
	return format(msg, '44', '49')
}

// bg_magenta formats the `msg` in magenta background.
pub fn bg_magenta(msg string) string {
	return format(msg, '45', '49')
}

// bg_cyan formats the `msg` in cyan background.
pub fn bg_cyan(msg string) string {
	return format(msg, '46', '49')
}

// bg_white formats the `msg` in white background.
pub fn bg_white(msg string) string {
	return format(msg, '47', '49')
}

// gray formats the `msg` in gray (equivalent to `bright_black`).
pub fn gray(msg string) string {
	return bright_black(msg)
}

// bright_black formats the `msg` in bright black.
pub fn bright_black(msg string) string {
	return format(msg, '90', '39')
}

// bright_red formats the `msg` in bright red.
pub fn bright_red(msg string) string {
	return format(msg, '91', '39')
}

// bright_green formats the `msg` in bright green.
pub fn bright_green(msg string) string {
	return format(msg, '92', '39')
}

// bright_yellow formats the `msg` in bright yellow.
pub fn bright_yellow(msg string) string {
	return format(msg, '93', '39')
}

// bright_blue formats the `msg` in bright blue.
pub fn bright_blue(msg string) string {
	return format(msg, '94', '39')
}

// bright_magenta formats the `msg` in bright magenta.
pub fn bright_magenta(msg string) string {
	return format(msg, '95', '39')
}

// bright_cyan formats the `msg` in bright cyan.
pub fn bright_cyan(msg string) string {
	return format(msg, '96', '39')
}

// bright_white formats the `msg` in bright white.
pub fn bright_white(msg string) string {
	return format(msg, '97', '39')
}

// bright_bg_black formats the `msg` in bright black background.
pub fn bright_bg_black(msg string) string {
	return format(msg, '100', '49')
}

// bright_bg_red formats the `msg` in bright red background.
pub fn bright_bg_red(msg string) string {
	return format(msg, '101', '49')
}

// bright_bg_green formats the `msg` in bright green background.
pub fn bright_bg_green(msg string) string {
	return format(msg, '102', '49')
}

// bright_bg_yellow formats the `msg` in bright yellow background.
pub fn bright_bg_yellow(msg string) string {
	return format(msg, '103', '49')
}

// bright_bg_blue formats the `msg` in bright blue background.
pub fn bright_bg_blue(msg string) string {
	return format(msg, '104', '49')
}

// bright_bg_magenta formats the `msg` in bright magenta background.
pub fn bright_bg_magenta(msg string) string {
	return format(msg, '105', '49')
}

// bright_bg_cyan formats the `msg` in bright cyan background.
pub fn bright_bg_cyan(msg string) string {
	return format(msg, '106', '49')
}

// bright_bg_white formats the `msg` in bright white background.
pub fn bright_bg_white(msg string) string {
	return format(msg, '107', '49')
}

// highlight_command highlights the command with an on-brand background to make CLI commands immediately recognizable.
pub fn highlight_command(command string) string {
	return bright_white(bg_cyan(' ${command} '))
}

pub enum TextStyle {
	bold      = 1
	dim       = 2
	italic    = 3
	underline = 4
	blink     = 5
	reverse   = 7
}

pub enum FgColor {
	black   = 30
	red     = 31
	green   = 32
	yellow  = 33
	blue    = 34
	magenta = 35
	cyan    = 36
	white   = 37
}

pub enum BgColor {
	black   = 40
	red     = 41
	green   = 42
	yellow  = 43
	blue    = 44
	magenta = 45
	cyan    = 46
	white   = 47
}

@[params]
pub struct ColorConfig {
pub mut:
	styles []TextStyle
	fg     ?FgColor
	bg     ?BgColor
	custom string
}

// write_color appends the ANSI colorful string `s` to the buffer.
pub fn write_color(mut b strings.Builder, s string, config ColorConfig) {
	mut codes := []string{cap: 3}

	for style in config.styles {
		codes << int(style).str()
	}

	if fg := config.fg {
		codes << int(fg).str()
	}

	if bg := config.bg {
		codes << int(bg).str()
	}

	if config.custom != '' {
		codes << config.custom
	}

	if codes.len > 0 {
		code_str := codes.join(';')
		b.write_string('\x1b[${code_str}m${s}\x1b[0m')
	} else {
		b.write_string(s)
	}
}

// writeln_color appends the ANSI colorful string `s`, and then a newline character.
pub fn writeln_color(mut b strings.Builder, s string, color ColorConfig) {
	write_color(mut b, s, color)
	b.writeln('')
}
