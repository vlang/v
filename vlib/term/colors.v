// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module term

pub fn format(msg string, open string, close string) string {
	return '\x1b[' + open + 'm' + msg + '\x1b[' + close + 'm'
}

pub fn format_rgb(r int, g int, b int, msg string, open string, close string) string {
	return '\x1b[' + open + ';2;' + r.str() + ';' + g.str() + ';' + b.str() + 'm' + msg + '\x1b[' +
		close + 'm'
}

pub fn rgb(r int, g int, b int, msg string) string {
	return format_rgb(r, g, b, msg, '38', '39')
}

pub fn bg_rgb(r int, g int, b int, msg string) string {
	return format_rgb(r, g, b, msg, '48', '49')
}

pub fn hex(hex int, msg string) string {
	return format_rgb(hex >> 16, hex >> 8 & 0xFF, hex & 0xFF, msg, '38', '39')
}

pub fn bg_hex(hex int, msg string) string {
	return format_rgb(hex >> 16, hex >> 8 & 0xFF, hex & 0xFF, msg, '48', '49')
}

pub fn bg_black(msg string) string {
	return format(msg, '40', '49')
}

pub fn bright_bg_black(msg string) string {
	return format(msg, '100', '49')
}

pub fn bg_blue(msg string) string {
	return format(msg, '44', '49')
}

pub fn bright_bg_blue(msg string) string {
	return format(msg, '104', '49')
}

pub fn bg_cyan(msg string) string {
	return format(msg, '46', '49')
}

pub fn bright_bg_cyan(msg string) string {
	return format(msg, '106', '49')
}

pub fn bg_green(msg string) string {
	return format(msg, '42', '49')
}

pub fn bright_bg_green(msg string) string {
	return format(msg, '102', '49')
}

pub fn bg_magenta(msg string) string {
	return format(msg, '45', '49')
}

pub fn bright_bg_magenta(msg string) string {
	return format(msg, '105', '49')
}

pub fn bg_red(msg string) string {
	return format(msg, '41', '49')
}

pub fn bright_bg_red(msg string) string {
	return format(msg, '101', '49')
}

pub fn bg_white(msg string) string {
	return format(msg, '47', '49')
}

pub fn bright_bg_white(msg string) string {
	return format(msg, '107', '49')
}

pub fn bg_yellow(msg string) string {
	return format(msg, '43', '49')
}

pub fn bright_bg_yellow(msg string) string {
	return format(msg, '103', '49')
}

pub fn black(msg string) string {
	return format(msg, '30', '39')
}

pub fn bright_black(msg string) string {
	return format(msg, '90', '39')
}

pub fn blue(msg string) string {
	return format(msg, '34', '39')
}

pub fn bright_blue(msg string) string {
	return format(msg, '94', '39')
}

pub fn bold(msg string) string {
	return format(msg, '1', '22')
}

pub fn cyan(msg string) string {
	return format(msg, '36', '39')
}

pub fn bright_cyan(msg string) string {
	return format(msg, '96', '39')
}

pub fn dim(msg string) string {
	return format(msg, '2', '22')
}

pub fn green(msg string) string {
	return format(msg, '32', '39')
}

pub fn bright_green(msg string) string {
	return format(msg, '92', '39')
}

pub fn gray(msg string) string {
	return bright_black(msg)
}

pub fn hidden(msg string) string {
	return format(msg, '8', '28')
}

pub fn italic(msg string) string {
	return format(msg, '3', '23')
}

pub fn inverse(msg string) string {
	return format(msg, '7', '27')
}

pub fn magenta(msg string) string {
	return format(msg, '35', '39')
}

pub fn bright_magenta(msg string) string {
	return format(msg, '95', '39')
}

pub fn reset(msg string) string {
	return format(msg, '0', '0')
}

pub fn red(msg string) string {
	return format(msg, '31', '39')
}

pub fn bright_red(msg string) string {
	return format(msg, '91', '39')
}

pub fn strikethrough(msg string) string {
	return format(msg, '9', '29')
}

pub fn underline(msg string) string {
	return format(msg, '4', '24')
}

pub fn white(msg string) string {
	return format(msg, '37', '39')
}

pub fn bright_white(msg string) string {
	return format(msg, '97', '39')
}

pub fn yellow(msg string) string {
	return format(msg, '33', '39')
}

pub fn bright_yellow(msg string) string {
	return format(msg, '93', '39')
}
