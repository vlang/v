module strings

// contains_any reports whether s contains any rune in chars.
pub fn contains_any(s string, chars string) bool {
	for _, c in s.runes() {
		for _, r in chars.runes() {
			if c == r {
				return true
			}
		}
	}
	return false
}
