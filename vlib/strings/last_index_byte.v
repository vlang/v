module strings

// last_index_byte returns the index of the last occurrence of c in s, or -1.
pub fn last_index_byte(s string, c u8) int {
	for i := s.len - 1; i >= 0; i-- {
		if s[i] == c {
			return i
		}
	}
	return -1
}
