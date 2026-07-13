module document

pub fn quote_string(s string) string {
	mut b := []u8{cap: s.len * 5 / 4 + 2}
	b << 34
	for r in s.runes() {
		if r == `"` || r == `\\` { b << 92 }
		if r == `\n` {
			b << 92
			b << 110
		} else if r == `\r` {
			b << 92
			b << 114
		} else if r == `\t` {
			b << 92
			b << 116
		} else if r == `\b` {
			b << 92
			b << 98
		} else if r == `\f` {
			b << 92
			b << 102
		} else if needs_unicode_escape(r) {
			b << 92
			b << 117
			b << 123
			for c in r.hex().bytes() {
				b << c
			}
			b << 125
		} else {
			for c in r.bytes() {
				b << c
			}
		}
	}
	b << 34
	return b.bytestr()
}

fn needs_unicode_escape(r rune) bool {
	return (r >= 0 && r <= 0x07) || r == 0x0b || (r >= 0x0e && r <= 0x1f)
		|| r == 0x7f || r == 0x85 || r == 0x2028 || r == 0x2029
		|| (r >= 0x200e && r <= 0x200f) || (r >= 0x202a && r <= 0x202e)
		|| (r >= 0x2066 && r <= 0x2069) || r == 0xfeff
}

pub fn raw_string(s string) string {
	mut hashes := 1
	for {
		if hashes > 20 { break
		 }
		marker := '"' + '#'.repeat(hashes)
		if !s.contains(marker) { break
		 }
		hashes++
	}
	return '#'.repeat(hashes) + '"' + s + '"' + '#'.repeat(hashes)
}

pub fn unquote_string(s string) !string {
	if s.len < 2 || s[0] != 34 || s[s.len - 1] != 34 {
		return error('kdl: invalid quoted string')
	}
	inner := s[1..s.len - 1]
	mut b := []u8{cap: inner.len}
	mut i := 0
	for i < inner.len {
		c := inner[i]
		if c == 92 {
			i++
			if i >= inner.len { return error('kdl: invalid escape at end of string') }
			if is_kdl_ws_or_newline_at(inner, i) {
				i = skip_kdl_ws_or_newline(inner, i)
				continue
			}
			next := inner[i]
			match next {
				110 {
					b << 10
				}
				114 {
					b << 13
				}
				116 {
					b << 9
				}
				98 {
					b << 8
				}
				102 {
					b << 12
				}
				115 {
					b << 32
				}
				117 {
					if i + 2 >= inner.len || inner[i + 1] != 123 {
						return error('kdl: invalid unicode escape')
					}
					i += 2
					mut hx := ''
					for i < inner.len && inner[i] != 125 {
						hx += inner[i].ascii_str()
						i++
					}
					if i >= inner.len { return error('kdl: unclosed unicode escape') }
					cp := parse_hex4_unicode(hx)!
					for bt in cp.bytes() {
						b << bt
					}
				}
				34 {
					b << 34
				}
				92 {
					b << 92
				}
				else {
					return error('kdl: invalid escape \\' + next.ascii_str())
				}
			}
		} else {
			b << c
		}
		i++
	}
	return b.bytestr()
}

fn parse_hex4_unicode(hx string) !string {
	if hx.len == 0 || hx.len > 6 { return error('kdl: unicode escape must have 1-6 hex digits') }
	mut val := u32(0)
	for c in hx.bytes() {
		val <<= 4
		if c >= 48 && c <= 57 {
			val |= u32(c - 48)
		} else if c >= 97 && c <= 102 {
			val |= u32(c - 97 + 10)
		} else if c >= 65 && c <= 70 {
			val |= u32(c - 65 + 10)
		} else {
			return error('kdl: invalid hex digit')
		}
	}
	if val > 0x10FFFF { return error('kdl: invalid unicode codepoint') }
	if val >= 0xD800 && val <= 0xDFFF { return error('kdl: unicode escape must not be a surrogate') }
	mut b := []u8{}
	if val < 0x80 {
		b << u8(val)
	} else if val < 0x800 {
		b << u8(0xC0 | (val >> 6))
		b << u8(0x80 | (val & 0x3F))
	} else if val < 0x10000 {
		b << u8(0xE0 | (val >> 12))
		b << u8(0x80 | ((val >> 6) & 0x3F))
		b << u8(0x80 | (val & 0x3F))
	} else {
		b << u8(0xF0 | (val >> 18))
		b << u8(0x80 | ((val >> 12) & 0x3F))
		b << u8(0x80 | ((val >> 6) & 0x3F))
		b << u8(0x80 | (val & 0x3F))
	}
	return b.bytestr()
}

fn is_kdl_ws_or_newline_at(s string, pos int) bool {
	if pos >= s.len {
		return false
	}
	b := s[pos]
	if b in [u8(0x09), 0x0a, 0x0b, 0x0c, 0x0d, 0x20] {
		return true
	}
	if b == 0xc2 && pos + 1 < s.len {
		return s[pos + 1] == 0x85 || s[pos + 1] == 0xa0
	}
	if b == 0xe1 && pos + 2 < s.len && s[pos + 1] == 0x9a && s[pos + 2] == 0x80 {
		return true
	}
	if b == 0xe2 && pos + 2 < s.len && s[pos + 1] == 0x80 {
		third := s[pos + 2]
		if (third >= 0x80 && third <= 0x8a) || third == 0xa8 || third == 0xa9 || third == 0xaf {
			return true
		}
	}
	if b == 0xe2 && pos + 2 < s.len && s[pos + 1] == 0x81 && s[pos + 2] == 0x9f {
		return true
	}
	return b == 0xe3 && pos + 2 < s.len && s[pos + 1] == 0x80 && s[pos + 2] == 0x80
}

fn skip_kdl_ws_or_newline(s string, start int) int {
	mut i := start
	for i < s.len && is_kdl_ws_or_newline_at(s, i) {
		i += utf8_len_from_start_byte(s[i])
	}
	return i
}

fn utf8_len_from_start_byte(b u8) int {
	if b < 0x80 {
		return 1
	}
	if b < 0xe0 {
		return 2
	}
	if b < 0xf0 {
		return 3
	}
	return 4
}
