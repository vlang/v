module strings

// reverse returns s reversed by runes.
pub fn reverse(s string) string {
	runes := s.runes()
	mut out := []rune{len: runes.len}
	for i, r in runes {
		out[runes.len - 1 - i] = r
	}
	return out.string()
}
