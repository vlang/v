module bytes

import encoding.utf8

// to_valid_utf8 replaces invalid UTF-8 sequences with replacement, keeping
// valid sequences intact. Mirrors Go's bytes.ToValidUTF8.
pub fn to_valid_utf8(b []u8, replacement []u8) []u8 {
	s := b.bytestr()
	if utf8.validate_str(s) {
		return b.clone()
	}
	mut out := []u8{cap: b.len}
	mut i := 0
	for i < s.len {
		r, size := utf8.decode_rune_at(s, i)
		if size == 0 {
			break
		}
		if r == rune(0xfffd) && size == 1 {
			// malformed byte: emit the replacement, consume one byte
			out << replacement
			i += 1
		} else {
			for j in 0 .. size {
				out << b[i + j]
			}
			i += size
		}
	}
	return out
}
