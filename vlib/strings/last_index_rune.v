module strings

// last_index_rune returns the byte index of the last occurrence of r in s, or -1.
pub fn last_index_rune(s string, r rune) int {
	mut idx := 0
	mut found := -1
	for _, c in s.runes() {
		if c == r {
			found = idx
		}
		if c < 0x80 {
			idx += 1
		} else if c < 0x800 {
			idx += 2
		} else if c < 0x10000 {
			idx += 3
		} else {
			idx += 4
		}
	}
	return found
}
