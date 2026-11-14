module strings

import encoding.utf8

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

// title returns a copy of s with the first character of each word (split by
// whitespace) converted to title case. Mirrors Go's strings.Title (deprecated
// but still useful as a simple helper).
pub fn title(s string) string {
	mut out := []u8{}
	mut prev_space := true
	for b in s.bytes() {
		if b.is_space() {
			out << b
			prev_space = true
		} else {
			if prev_space && b >= `a` && b <= `z` {
				out << b - 32
			} else {
				out << b
			}
			prev_space = false
		}
	}
	return out.bytestr()
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

// to_valid_utf8 replaces invalid UTF-8 sequences with replacement.
pub fn to_valid_utf8(s string, replacement string) string {
	if utf8.validate_str(s) {
		return s
	}
	// fallback: rebuild valid runes
	mut out := []u8{}
	for b in s.bytes() {
		if b < 0x80 {
			out << b
		} else {
			out << replacement.bytes()
		}
	}
	return out.bytestr()
}

// Reader implements io.Reader for a string.
pub struct Reader {
mut:
	s   string
	pos int
}

pub fn new_reader(s string) Reader {
	return Reader{ s: s }
}

pub fn (mut r Reader) read(mut buf []u8) !int {
	if r.pos >= r.s.len {
		return 0
	}
	n := if buf.len < r.s.len - r.pos { buf.len } else { r.s.len - r.pos }
	for i in 0 .. n {
		buf[i] = r.s[r.pos + i]
	}
	r.pos += n
	return n
}

pub fn (r Reader) len() int {
	return r.s.len - r.pos
}

pub fn (mut r Reader) reset(s string) {
	r.s = s
	r.pos = 0
}
