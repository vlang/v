module strings

// split_after splits s at every occurrence of sep, keeping sep at the end of
// each piece (Go's strings.SplitAfter). The final piece has no separator.
// An empty sep returns [s] (Go splits per UTF-8 sequence there; vlib keeps
// the simple, documented behavior).
pub fn split_after(s string, sep string) []string {
	if sep == '' {
		return [s]
	}
	mut out := []string{}
	mut rest := s
	for {
		if i := rest.index(sep) {
			out << rest[..i + sep.len]
			rest = rest[i + sep.len..]
		} else {
			out << rest
			break
		}
	}
	return out
}

// equal_fold reports whether a and b are equal under ASCII case folding
// (Go's strings.EqualFold, ASCII subset). Non-ASCII bytes compare exactly,
// so it is allocation-free unlike a.to_lower() == b.to_lower().
pub fn equal_fold(a string, b string) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		mut ca := a[i]
		mut cb := b[i]
		if ca == cb {
			continue
		}
		if ca >= `A` && ca <= `Z` {
			ca += 32
		}
		if cb >= `A` && cb <= `Z` {
			cb += 32
		}
		if ca != cb {
			return false
		}
	}
	return true
}

// last_index_any returns the byte index of the last occurrence in s of any
// byte of chars, or -1 if there is none.
pub fn last_index_any(s string, chars string) int {
	if chars == '' {
		return -1
	}
	for i := s.len - 1; i >= 0; i-- {
		if chars.index_u8(s[i]) >= 0 {
			return i
		}
	}
	return -1
}

// contains_rune reports whether s contains the rune r anywhere.
pub fn contains_rune(s string, r rune) bool {
	for c in s.runes() {
		if c == r {
			return true
		}
	}
	return false
}
