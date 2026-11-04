module strings

// count counts non-overlapping instances of substr in s.
pub fn count(s string, substr string) int {
	if substr == '' {
		return s.len + 1
	}
	mut n := 0
	mut pos := 0
	for pos <= s.len {
		i := s[pos..].index(substr) or { break }
		n++
		pos += i + substr.len
	}
	return n
}
