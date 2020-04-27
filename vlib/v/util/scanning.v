module util

import os

[inline]
fn is_name_char(c byte) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_`
}

[inline]
fn is_nl(c byte) bool {
	return c == `\r` || c == `\n`
}

fn contains_capital(s string) bool {
	for c in s {
		if c >= `A` && c <= `Z` {
			return true
		}
	}
	return false
}

// HTTPRequest  bad
// HttpRequest  good
fn good_type_name(s string) bool {
	if s.len < 4 {
		return true
	}
	for i in 2 .. s.len {
		if s[i].is_capital() && s[i - 1].is_capital() && s[i - 2].is_capital() {
			return false
		}
	}
	return true
}

pub fn cescaped_path(s string) string {
	return s.replace('\\', '\\\\')
}

pub fn is_fmt() bool {
	return os.executable().contains('vfmt')
}
