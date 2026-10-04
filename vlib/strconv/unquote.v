module strconv

// UnquoteChar is the result of unquote_char: the rune that was decoded,
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

// index_byte returns the index of the first c in s, or -1.
fn index_byte(s string, c u8) int {
	for i, b in s.bytes() {
		if b == c {
			return i
		}
	}
	return -1
}

// contains_byte reports whether c appears in s.
fn contains_byte(s string, c u8) bool {
	return index_byte(s, c) >= 0
}

// valid_string reports whether s is well-formed UTF-8. Go uses
// utf8.ValidString here, and an empty string is valid.
fn valid_string(s string) bool {
	mut rest := s
	mut n := 0
	for n < s.len {
		r, size := decode_rune_in_string(rest)
		if r == rune_error && size == 1 {
			return false
		}
		rest = rest[size..]
		n += size
	}
	return true
}

// append_rune encodes r as UTF-8 and appends it to buf.
fn append_rune(mut buf []u8, r rune) {
	x := u32(r)
	if r < 0x80 {
		buf << u8(x)
	} else if r < 0x800 {
		buf << u8(0xC0 | x >> 6)
		buf << u8(0x80 | x & 0x3F)
	} else if r < 0x10000 {
		buf << u8(0xE0 | x >> 12)
		buf << u8(0x80 | (x >> 6) & 0x3F)
		buf << u8(0x80 | x & 0x3F)
	} else {
		buf << u8(0xF0 | x >> 18)
		buf << u8(0x80 | (x >> 12) & 0x3F)
		buf << u8(0x80 | (x >> 6) & 0x3F)
		buf << u8(0x80 | x & 0x3F)
	}
}

// hex_value returns the numeric value of a hexadecimal digit.
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
// quote must be a single quote byte, '"' or '\''.
pub fn unquote_char(s string, quote u8) !UnquoteCharResult {
	if s.len == 0 {
		return error(syntax_error())
	}
	c0 := s.bytes()[0]
	if c0 == quote && (quote == 0x27 || quote == 0x22) {
		// the quote character itself cannot appear unescaped
		return error(syntax_error())
	}
	if c0 >= 0x80 {
		r, size := decode_rune_in_string(s)
		return UnquoteCharResult{
			value:     r
			multibyte: true
			tail:      s[size..]
		}
	}
	if c0 != 0x5C {
		return UnquoteCharResult{
			value: rune(c0)
			tail:  s[1..]
		}
	}

	// the hard case: c0 is a backslash
	if s.len <= 1 {
		return error(syntax_error())
	}
	c := s.bytes()[1]
	mut rest := s[2..]
	mut value := rune(0)
	mut multibyte := false

	match c {
		0x61 {
			value = 0x07
		}
		0x62 {
			value = 0x08
		}
		0x66 {
			value = 0x0C
		}
		0x6E {
			value = 0x0A
		}
		0x72 {
			value = 0x0D
		}
		0x74 {
			value = 0x09
		}
		0x76 {
			value = 0x0B
		}
		0x78, 0x75, 0x55 {
			n := if c == 0x78 {
				2
			} else if c == 0x75 {
				4
			} else {
				8
			}
			if rest.len < n {
				return error(syntax_error())
			}
			mut v := rune(0)
			for j in 0 .. n {
				x := hex_value(rest.bytes()[j])
				if x < 0 {
					return error(syntax_error())
				}
				v = rune(u32(v) << 4) | rune(x)
			}
			rest = rest[n..]
			if c == 0x78 {
				// \x yields a single byte, which need not be valid UTF-8
				value = v
			} else {
				if !is_valid_rune(v) {
					return error(syntax_error())
				}
				value = v
				multibyte = true
			}
		}
		0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37 {
			mut v := rune(c) - rune(`0`)
			if rest.len < 2 {
				return error(syntax_error())
			}
			for j in 0 .. 2 {
				x := rune(rest.bytes()[j]) - rune(`0`)
				if x < 0 || x > 7 {
					return error(syntax_error())
				}
				v = rune(u32(v) << 3) | x
			}
			rest = rest[2..]
			if v > 255 {
				return error(syntax_error())
			}
			value = v
		}
		0x5C {
			value = 0x5C
		}
		0x27, 0x22 {
			if c != quote {
				return error(syntax_error())
			}
			value = rune(c)
		}
		else {
			return error(syntax_error())
		}
	}
	return UnquoteCharResult{
		value:     value
		multibyte: multibyte
		tail:      rest
	}
}

// UnquoteResult carries what unquote_prefix parses: the consumed part and
// whatever was left over.
struct UnquoteResult {
	out string
	rem string
}

// unquote_prefix parses one quoted literal at the start of the input. When
// unescape is true the value is unescaped, otherwise the matched text is
// returned verbatim, quotes included.
fn unquote_prefix(src string, unescape bool) !UnquoteResult {
	// A `return` directly followed by a composite literal is ambiguous to the
	// parser here, so each return below binds a named value first.

	if src.len < 2 {
		return error(syntax_error())
	}
	qch := src.bytes()[0]
	mut end := index_byte(src[1..], qch)
	if end < 0 {
		return error(syntax_error())
	}
	end += 2 // one past the closing qch; wrong if escapes are present

	if qch == 0x60 {
		// a raw literal, delimited by backquotes
		if !unescape {
			res := UnquoteResult{
				out: src[..end]
				rem: src[end..]
			}
			return res
		}
		body := src[1..end - 1]
		if !contains_byte(body, 0x0D) {
			res := UnquoteResult{
				out: body
				rem: src[end..]
			}
			return res
		}
		// a carriage return inside a raw literal is discarded
		mut buf := []u8{}
		for b in body.bytes() {
			if b != 0x0D {
				buf << b
			}
		}
		res := UnquoteResult{
			out: buf.bytestr()
			rem: src[end..]
		}
		return res
	}

	if qch != 0x22 && qch != 0x27 {
		return error(syntax_error())
	}

	head := src[..end]
	// the fast path: no escapes and no unescaped newline
	if !contains_byte(head, 0x5C) && !contains_byte(head, 0x0A) {
		body := src[1..end - 1]
		mut valid := false
		if qch == 0x22 {
			valid = valid_string(body)
		} else {
			r, n := decode_rune_in_string(body)
			valid = n == body.len && (r != rune_error || n != 1)
		}
		if valid {
			res := UnquoteResult{
				out: if unescape { body } else { head }
				rem: src[end..]
			}
			return res
		}
	}

	// the slow path: at least one escape sequence
	mut buf := []u8{}
	in0 := src
	mut cur := src[1..]
	mut ok := false
	for cur.len > 0 && cur.bytes()[0] != qch {
		if cur.bytes()[0] == 0x0A {
			// an unescaped newline is never valid
			return error(syntax_error())
		}
		res := unquote_char(cur, qch) or { return error(syntax_error()) }
		cur = res.tail
		if unescape {
			if res.value < 0x80 || !res.multibyte {
				buf << u8(res.value)
			} else {
				append_rune(mut buf, res.value)
			}
		}
		if qch == 0x27 {
			// a single-quoted literal holds exactly one character
			break
		}
	}
	if cur.len > 0 && cur.bytes()[0] == qch {
		cur = cur[1..]
		ok = true
	}
	if !ok {
		return error(syntax_error())
	}
	if unescape {
		res := UnquoteResult{
			out: buf.bytestr()
			rem: cur
		}
		return res
	}
	res := UnquoteResult{
		out: in0[..in0.len - cur.len]
		rem: cur
	}
	return res
}

// quoted_prefix returns the quoted literal at the start of s, verbatim and
// including its quotes. It fails when s does not begin with a valid literal.
pub fn quoted_prefix(s string) !string {
	r := unquote_prefix(s, false) or { return error(syntax_error()) }
	return r.out
}

// unquote returns the string value that s quotes. s may be single-quoted,
// double-quoted or backquoted; a single-quoted literal yields the one
// character it holds.
pub fn unquote(s string) !string {
	r := unquote_prefix(s, true) or { return error(syntax_error()) }
	if r.rem.len > 0 {
		return error(syntax_error())
	}
	return r.out
}
