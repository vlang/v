module strconv

// UnquoteCharResult is the result of unquote_char: the rune that was decoded,
// whether it was written as a multi-byte sequence, and whatever followed it.
pub struct UnquoteCharResult {
pub:
	value     rune
	multibyte bool
	tail      string
}

// syntax_error is the single failure this file can report, matching Go's
// strconv.ErrSyntax. Every malformed input yields the same error there, so
// there is nothing to distinguish here either.
fn syntax_error() string {
	return 'strconv: invalid syntax'
}

// index_byte_in returns the index of the first c in s[start..end], or -1.
fn index_byte_in(s string, c u8, start int, end int) int {
	for i in start .. end {
		if s[i] == c {
			return i
		}
	}
	return -1
}

// hex_value returns the numeric value of a hexadecimal digit, or -1.
fn hex_value(c u8) int {
	if c >= `0` && c <= `9` {
		return int(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return int(c - `a`) + 10
	}
	if c >= `A` && c <= `F` {
		return int(c - `A`) + 10
	}
	return -1
}

// unquote_char decodes the leading character of s, which must be a valid
// literal body for the given quote byte, and returns the rest of s unchanged.
// It fails when s does not begin with a decodable character.
//
// quote is the quote byte of the literal s comes from. With `'` it accepts the
// escape `\'` and rejects an unescaped `'`, with `"` likewise for `"`, and with
// any other byte, such as 0, it accepts neither escape and both quotes as is.
pub fn unquote_char(s string, quote u8) !UnquoteCharResult {
	value, multibyte, next := unquote_char_at(s, 0, quote)!
	return UnquoteCharResult{
		value:     value
		multibyte: multibyte
		tail:      s[next..]
	}
}

// unquote_char_at decodes the character that starts at byte i of s, like
// unquote_char(s[i..], quote), and returns the index just past it instead of
// the tail, so that a caller walking a whole literal never copies its rest.
fn unquote_char_at(s string, i int, quote u8) !(rune, bool, int) {
	if i >= s.len {
		return error(syntax_error())
	}
	c0 := s[i]
	if c0 == quote && (quote == `'` || quote == `"`) {
		// the quote character itself cannot appear unescaped
		return error(syntax_error())
	}
	if c0 >= 0x80 {
		r, size := decode_rune_at(s, i)
		return r, true, i + size
	}
	if c0 != `\\` {
		return rune(c0), false, i + 1
	}

	// the hard case: c0 is a backslash
	if i + 1 >= s.len {
		return error(syntax_error())
	}
	c := s[i + 1]
	mut next := i + 2
	match c {
		`a` {
			return 0x07, false, next
		}
		`b` {
			return 0x08, false, next
		}
		`f` {
			return 0x0C, false, next
		}
		`n` {
			return 0x0A, false, next
		}
		`r` {
			return 0x0D, false, next
		}
		`t` {
			return 0x09, false, next
		}
		`v` {
			return 0x0B, false, next
		}
		`x`, `u`, `U` {
			n := if c == `x` {
				2
			} else if c == `u` {
				4
			} else {
				8
			}
			if s.len - next < n {
				return error(syntax_error())
			}
			mut v := rune(0)
			for j in 0 .. n {
				x := hex_value(s[next + j])
				if x < 0 {
					return error(syntax_error())
				}
				v = rune(u32(v) << 4) | rune(x)
			}
			next += n
			if c == `x` {
				// \x yields a single byte, which need not be valid UTF-8
				return v, false, next
			}
			if !is_valid_rune(v) {
				return error(syntax_error())
			}
			return v, true, next
		}
		`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7` {
			mut v := rune(c) - rune(`0`)
			if s.len - next < 2 {
				return error(syntax_error())
			}
			for j in 0 .. 2 {
				x := rune(s[next + j]) - rune(`0`)
				if x < 0 || x > 7 {
					return error(syntax_error())
				}
				v = rune(u32(v) << 3) | x
			}
			next += 2
			if v > 255 {
				return error(syntax_error())
			}
			return v, false, next
		}
		`\\` {
			return `\\`, false, next
		}
		`'`, `"` {
			if c != quote {
				return error(syntax_error())
			}
			return rune(c), false, next
		}
		else {
			return error(syntax_error())
		}
	}
}

// unquote_prefix parses one quoted literal at the start of src. When unescape
// is true it returns the value the literal spells, otherwise the literal
// itself, quotes included. The second result is the index just past the
// literal.
fn unquote_prefix(src string, unescape bool) !(string, int) {
	if src.len < 2 {
		return error(syntax_error())
	}
	qch := src[0]
	mut end := index_byte_in(src, qch, 1, src.len)
	if end < 0 {
		return error(syntax_error())
	}
	end++ // one past the closing qch; wrong if escapes are present

	if qch == `\`` {
		// a raw literal, delimited by backquotes
		if !unescape {
			return src[..end], end
		}
		if index_byte_in(src, `\r`, 1, end - 1) < 0 {
			return src[1..end - 1], end
		}
		// a carriage return inside a raw literal is discarded
		mut buf := []u8{cap: end - 2}
		for i in 1 .. end - 1 {
			if src[i] != `\r` {
				buf << src[i]
			}
		}
		return buf.bytestr(), end
	}

	if qch != `"` && qch != `'` {
		return error(syntax_error())
	}

	// the fast path: no escapes and no unescaped newline
	if index_byte_in(src, `\\`, 1, end) < 0 && index_byte_in(src, `\n`, 1, end) < 0 {
		mut valid := false
		if qch == `"` {
			valid = true
			mut i := 1
			for i < end - 1 {
				r, size := decode_rune_at(src, i)
				if r == rune_error && size == 1 {
					valid = false
					break
				}
				i += size
			}
		} else {
			r, n := decode_rune_at(src, 1)
			valid = 1 + n + 1 == end && (r != rune_error || n != 1)
		}
		if valid {
			if unescape {
				return src[1..end - 1], end
			}
			return src[..end], end
		}
	}

	// the slow path: at least one escape sequence
	mut buf := []u8{}
	if unescape {
		buf = []u8{cap: 3 * end / 2}
	}
	mut i := 1
	for i < src.len && src[i] != qch {
		if src[i] == `\n` {
			// an unescaped newline is never valid
			return error(syntax_error())
		}
		r, multibyte, next := unquote_char_at(src, i, qch) or { return error(syntax_error()) }
		i = next
		if unescape {
			if r < 0x80 || !multibyte {
				buf << u8(r)
			} else {
				append_rune(mut buf, r)
			}
		}
		if qch == `'` {
			// a single-quoted literal holds exactly one character
			break
		}
	}
	if i >= src.len || src[i] != qch {
		return error(syntax_error())
	}
	i++ // skip the closing quote
	if unescape {
		return buf.bytestr(), i
	}
	return src[..i], i
}

// quoted_prefix returns the quoted literal at the start of s, verbatim and
// including its quotes. It fails when s does not begin with a valid literal.
pub fn quoted_prefix(s string) !string {
	out, _ := unquote_prefix(s, false) or { return error(syntax_error()) }
	return out
}

// unquote returns the string value that s quotes. s may be a Go-syntax
// single-quoted, double-quoted or backquoted literal; a single-quoted literal
// yields the one character it holds.
pub fn unquote(s string) !string {
	out, end := unquote_prefix(s, true) or { return error(syntax_error()) }
	if end != s.len {
		return error(syntax_error())
	}
	return out
}
