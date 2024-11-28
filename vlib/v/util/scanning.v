module util

pub const name_char_table = get_name_char_table()

pub const func_char_table = get_func_char_table()

pub const non_whitespace_table = get_non_white_space_table()

fn get_non_white_space_table() [256]bool {
	mut bytes := [256]bool{}
	for c in 0 .. 256 {
		bytes[c] = !u8(c).is_space()
	}
	return bytes
}

fn get_name_char_table() [256]bool {
	mut res := [256]bool{}
	for c in 0 .. 256 {
		res[c] = u8(c).is_letter() || c == `_`
	}
	return res
}

fn get_func_char_table() [256]bool {
	mut res := [256]bool{}
	for c in 0 .. 256 {
		res[c] = u8(c).is_letter() || u8(c).is_digit() || c == `_`
	}
	return res
}

@[direct_array_access; inline]
pub fn is_name_char(c u8) bool {
	return name_char_table[c]
}

@[direct_array_access; inline]
pub fn is_func_char(c u8) bool {
	return func_char_table[c]
}

pub fn contains_capital(s string) bool {
	for c in s {
		if c.is_capital() {
			return true
		}
	}
	return false
}

// HTTPRequest  bad
// HttpRequest  good
@[direct_array_access]
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
@[direct_array_access; inline]
pub fn is_generic_type_name(name string) bool {
	return name.len == 1 && name[0] != `C` && (name[0] >= `A` && name[0] <= `Z`)
}

pub fn cescaped_path(s string) string {
	return s.replace('\\', '\\\\')
}
