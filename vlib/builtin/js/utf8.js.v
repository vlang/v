module builtin

pub fn utf8_str_visible_length(s string) int {
	// todo: proper implementation
	res := 0
	#res.val = s.str.length;

	return res
}

pub fn utf8_str_len(s string) int {
	return s.len
}

pub fn utf8_char_len(b u8) int {
	return ((0xe5000000 >> ((b >> 3) & 0x1e)) & 3) + 1
}
