module strings

import encoding.utf8

// to_valid_utf8 replaces invalid UTF-8 sequences with replacement.
pub fn to_valid_utf8(s string, replacement string) string {
	if utf8.validate_str(s) {
		return s
	}
	// fallback: rebuild valid runes
	mut out := []u8{}
	for b in s.bytes() {
		if b < 0x80 {
			out << b
		} else {
			out << replacement.bytes()
		}
	}
	return out.bytestr()
}
