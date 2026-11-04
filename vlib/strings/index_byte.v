module strings

// index_byte returns the index of the first occurrence of c in s, or -1.
pub fn index_byte(s string, c u8) int {
	for i in 0 .. s.len {
		if s[i] == c {
			return i
		}
	}
	return -1
}
