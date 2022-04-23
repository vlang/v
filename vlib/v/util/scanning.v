module util

[inline]
pub fn is_name_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_`
}

[inline]
pub fn is_func_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_` || (c >= `0` && c <= `9`)
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
[direct_array_access]
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
[direct_array_access; inline]
pub fn is_generic_type_name(name string) bool {
	return name.len == 1 && name[0] != `C` && (name[0] >= `A` && name[0] <= `Z`)
}

pub fn cescaped_path(s string) string {
	return s.replace('\\', '\\\\')
}
