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

// fields splits s around each instance of one or more consecutive Unicode
// (here, ASCII) whitespace characters, returning a slice of the substrings of
// s or an empty slice if s contains only whitespace. Mirrors Go's strings.Fields.
pub fn fields(s string) []string {
	mut out := []string{}
	mut start := -1
	for i, c in s {
		if c.is_space() {
			if start >= 0 {
				out << s[start..i]
				start = -1
			}
		} else {
			if start < 0 {
				start = i
			}
		}
	}
	if start >= 0 {
		out << s[start..]
	}
	return out
}

// fields_func splits s at each run of code points c satisfying f(c), returning
// a slice of substrings of s. Mirrors Go's strings.FieldsFunc.
pub fn fields_func(s string, f fn (rune) bool) []string {
	mut out := []string{}
	mut start := -1
	for i, c in s {
		if f(c) {
			if start >= 0 {
				out << s[start..i]
				start = -1
			}
		} else {
			if start < 0 {
				start = i
			}
		}
	}
	if start >= 0 {
		out << s[start..]
	}
	return out
}
