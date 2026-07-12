module document

pub fn quote_string(s string) string {
	mut b := []u8{cap: s.len * 5 / 4 + 2}
	b << 34
	for c in s.bytes() {
		if c == 34 || c == 92 { b << 92 }
		if c == 10 {
			b << 92
			b << 110
		} else if c == 13 {
			b << 92
			b << 114
		} else if c == 9 {
			b << 92
			b << 116
		} else if c == 8 {
			b << 92
			b << 98
		} else if c == 12 {
			b << 92
			b << 102
		} else if c < 32 {
			b << 92
			b << 117
			b << 123
			hex4(c, mut b)
			b << 125
		} else {
			b << c
		}
	}
	b << 34
	return b.bytestr()
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
				10, 13 {
					for i < inner.len
						&& (inner[i] == 32 || inner[i] == 9 || inner[i] == 10 || inner[i] == 13) {
						i++
					}
					continue
				}
				else {
					b << 92
					b << next
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

fn hex4(v u8, mut b []u8) {
	hx := [u8(48), 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102]
	b << hx[(v >> 4) & 0xF]
	b << hx[v & 0xF]
}
