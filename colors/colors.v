// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module colors

fn format(msg, open, close string) string {
    return '\x1b[' + open + 'm' + msg + '\x1b[' + close + 'm'
}

fn bg_black(msg string) string {
    return format(msg, '40', '49')
}

fn bg_blue(msg string) string {
    return format(msg, '44', '49')
}

fn bg_cyan(msg string) string {
    return format(msg, '46', '49')
}

fn bg_green(msg string) string {
    return format(msg, '42', '49')
}

fn bg_magenta(msg string) string {
    return format(msg, '45', '49')
}

fn bg_red(msg string) string {
    return format(msg, '41', '49')
}

fn bg_white(msg string) string {
    return format(msg, '47', '49')
}

fn bg_yellow(msg string) string {
    return format(msg, '43', '49')
}

fn black(msg string) string {
    return format(msg, '30', '39')
}

fn blue(msg string) string {
    return format(msg, '34', '39')
}

fn bold(msg string) string {
    return format(msg, '1', '22')
}

fn cyan(msg string) string {
    return format(msg, '36', '39')
}

fn dim(msg string) string {
    return format(msg, '2', '22')
}

fn green(msg string) string {
    return format(msg, '32', '39')
}

fn gray(msg string) string {
    return format(msg, '90', '39')
}

fn hidden(msg string) string {
    return format(msg, '8', '28')
}

fn italic(msg string) string {
    return format(msg, '3', '23')
}

fn inverse(msg string) string {
    return format(msg, '7', '27')
}

fn magenta(msg string) string {
    return format(msg, '35', '39')
}

fn reset(msg string) string {
    return format(msg, '0', '0')
}

fn red(msg string) string {
    return format(msg, '31', '39')
}

fn strikethrough(msg string) string {
    return format(msg, '9', '29')
}

fn underline(msg string) string {
    return format(msg, '4', '24')
}

fn white(msg string) string {
    return format(msg, '37', '39')
}

fn yellow(msg string) string {
    return format(msg, '33', '39')
}
