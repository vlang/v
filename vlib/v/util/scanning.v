module util

[inline]
pub fn is_name_char(c byte) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_`
}

[inline]
pub fn is_func_char(c byte) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_` || c.is_digit()
}

[inline]
pub fn is_nl(c byte) bool {
	return c == `\r` || c == `\n`
}

pub fn contains_capital(s string) bool {
	for c in s {
		if c >= `A` && c <= `Z` {
			return true
		}
	}
	return false
}

// HTTPRequest  bad
// HttpRequest  good
pub fn good_type_name(s string) bool {
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

// is_generic_type_name returns true if the current token is a generic type name.
[inline]
pub fn is_generic_type_name(name string) bool {
	return name.len == 1 && name.is_capital() && name != 'C'
}

pub fn cescaped_path(s string) string {
	return s.replace('\\', '\\\\')
}
