module strings

// count_rune counts occurrences of r in s.
pub fn count_rune(s string, r rune) int {
	mut n := 0
	for _, c in s.runes() {
		if c == r {
			n++
		}
	}
	return n
}
