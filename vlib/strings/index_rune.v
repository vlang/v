module strings

// index_rune returns the byte index of the first occurrence of r in s, or -1.
pub fn index_rune(s string, r rune) int {
	// simple byte scan over runes
	mut idx := 0
	for _, c in s.runes() {
		if c == r {
			return idx
		}
		// advance idx by UTF-8 length of c
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
	return -1
}
