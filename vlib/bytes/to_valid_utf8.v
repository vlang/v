module bytes

import encoding.utf8

// to_valid_utf8 replaces invalid UTF-8 sequences with replacement.
pub fn to_valid_utf8(b []u8, replacement []u8) []u8 {
	if utf8.validate_str(b.bytestr()) {
		return b.clone()
	}
	mut out := []u8{}
	for byte in b {
		if byte < 0x80 {
			out << byte
		} else {
			out << replacement
		}
	}
	return out
}
