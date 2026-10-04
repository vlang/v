module strconv

import strings

const lower_hex = '0123456789abcdef'

// rune_error is what an invalid UTF-8 sequence decodes to, matching Go's
// utf8.RuneError.
const rune_error = 0xFFFD

// max_rune is the highest valid code point.
const max_rune = 0x10FFFF

// bsearch_u16 returns the index of the first element of s that is >= v, together
// with whether that element equals v. The tables are flat sorted lists of values,
// so this is a lower-bound search rather than a range search.
fn bsearch_u16(s []u16, v u16) (int, bool) {
	mut i := 0
	mut j := s.len
	for i < j {
		h := i + (j - i) >> 1
		if s[h] < v {
			i = h + 1
		} else {
			j = h
		}
	}
	return i, i < s.len && s[i] == v
}

// bsearch_u32 is bsearch_u16 for the 32 bit table.
fn bsearch_u32(s []u32, v u32) (int, bool) {
	mut i := 0
	mut j := s.len
	for i < j {
		h := i + (j - i) >> 1
		if s[h] < v {
			i = h + 1
		} else {
			j = h
		}
	}
	return i, i < s.len && s[i] == v
}

// is_print reports whether r is defined as printable by Go, meaning it is in
// category L, M, N, P, S or the ASCII space.
pub fn is_print(r rune) bool {
	// Fast check for Latin-1
	if r <= 0xFF {
		if r >= 0x20 && r <= 0x7E {
			return true
		}
		if r >= 0xA1 && r <= 0xFF {
			// ...except for the bizarre soft hyphen.
			return r != 0xAD
		}
		return false
	}

	// Find the first i such that the table entry is >= r. The start of a pair is
	// at an even index and the end at an odd one, so i&^1 and i|1 bracket the
	// pair that might span r. Finding r inside a pair is not enough: it then has
	// to be absent from the not-printable list.
	if r >= 0 && r < 1 << 16 {
		rr := u16(r)
		i, _ := bsearch_u16(is_print16, rr)
		if i >= is_print16.len || rr < is_print16[i & ~1] || is_print16[i | 1] < rr {
			return false
		}
		_, found := bsearch_u16(is_not_print16, rr)
		return !found
	}

	rr := u32(r)
	i, _ := bsearch_u32(is_print32, rr)
	if i >= is_print32.len || rr < is_print32[i & ~1] || is_print32[i | 1] < rr {
		return false
	}
	if r >= 0x20000 {
		return true
	}
	_, found := bsearch_u16(is_not_print32, u16(r - 0x10000))
	return !found
}

// is_in_graphic_list reports whether r is in the is_graphic list. Kept separate
// from is_graphic so that quoting can skip a second is_print call.
fn is_in_graphic_list(r rune) bool {
	// the list only covers the basic multilingual plane
	if r > 0xFFFF {
		return false
	}
	_, found := bsearch_u16(is_graphic, u16(r))
	return found
}

// is_graphic reports whether r is defined as graphic by Unicode: the printable
// characters plus the spaces, combining marks and format characters.
pub fn is_graphic(r rune) bool {
	if is_print(r) {
		return true
	}
	return is_in_graphic_list(r)
}

// decode_rune_in_string decodes the first rune of s, returning it and the number
// of bytes it occupies. An invalid sequence decodes to rune_error with a width of
// 1, so a caller walking a string always makes progress.
fn decode_rune_in_string(s string) (rune, int) {
	if s.len == 0 {
		return rune_error, 0
	}
	b0 := s[0]
	if b0 < 0x80 {
		return rune(b0), 1
	}
	// The second byte has a narrower legal range for the edges of each length,
	// which is what rules out overlong forms and surrogates.
	mut sz := 0
	mut lo := u8(0x80)
	mut hi := u8(0xBF)
	if b0 >= 0xC2 && b0 <= 0xDF {
		sz = 2
	} else if b0 >= 0xE0 && b0 <= 0xEF {
		sz = 3
		if b0 == 0xE0 {
			lo = 0xA0
		}
		if b0 == 0xED {
			hi = 0x9F
		}
	} else if b0 >= 0xF0 && b0 <= 0xF4 {
		sz = 4
		if b0 == 0xF0 {
			lo = 0x90
		}
		if b0 == 0xF4 {
			hi = 0x8F
		}
	} else {
		return rune_error, 1
	}
	if s.len < sz {
		return rune_error, 1
	}
	b1 := s[1]
	if b1 < lo || b1 > hi {
		return rune_error, 1
	}
	mut r := rune(b0 & (0x7F >> sz)) << 6 | rune(b1 & 0x3F)
	if sz >= 3 {
		b2 := s[2]
		if b2 < 0x80 || b2 > 0xBF {
			return rune_error, 1
		}
		r = rune(u32(r) << 6) | rune(b2 & 0x3F)
	}
	if sz == 4 {
		b3 := s[3]
		if b3 < 0x80 || b3 > 0xBF {
			return rune_error, 1
		}
		r = rune(u32(r) << 6) | rune(b3 & 0x3F)
	}
	return r, sz
}

// is_valid_rune reports whether r is a legal code point, matching Go's
// utf8.ValidRune: in range and not a surrogate half.
fn is_valid_rune(r rune) bool {
	return r >= 0 && r <= max_rune && !(r >= 0xD800 && r <= 0xDFFF)
}

// can_backquote reports whether s can be written unchanged between backquotes.
pub fn can_backquote(s string) bool {
	mut rest := s
	for rest.len > 0 {
		r, width := decode_rune_in_string(rest)
		rest = rest[width..]
		if width > 1 {
			if r == 0xFEFF {
				// BOMs are invisible and should not be quoted.
				return false
			}
			// All other multibyte runes are correctly encoded and assumed printable.
			continue
		}
		if r == rune_error {
			return false
		}
		if (r < 0x20 && r != 0x09) || r == 0x60 || r == 0x7F {
			return false
		}
	}
	return true
}

// append_escaped_rune writes r to b, escaping it if it cannot appear literally.
fn append_escaped_rune(mut b strings.Builder, r rune, quote u8, ascii_only bool, graphic_only bool) {
	if r == rune(quote) || r == 0x5C {
		// the quote character and the backslash are always escaped
		b.write_byte(0x5C)
		b.write_byte(u8(r))
		return
	}
	if ascii_only {
		if r < 0x80 && is_print(r) {
			b.write_byte(u8(r))
			return
		}
	} else if is_print(r) || (graphic_only && is_in_graphic_list(r)) {
		b.write_rune(r)
		return
	}
	match r {
		0x07 {
			b.write_string('\\a')
		}
		0x08 {
			b.write_string('\\b')
		}
		0x0C {
			b.write_string('\\f')
		}
		0x0A {
			b.write_string('\\n')
		}
		0x0D {
			b.write_string('\\r')
		}
		0x09 {
			b.write_string('\\t')
		}
		0x0B {
			b.write_string('\\v')
		}
		else {
			if r < 0x20 || r == 0x7F {
				b.write_string('\\x')
				b.write_byte(lower_hex[u8(r) >> 4])
				b.write_byte(lower_hex[u8(r) & 0xF])
				return
			}
			if !is_valid_rune(r) {
				// an out-of-range rune escapes as the replacement character
				b.write_string('\\ufffd')
				return
			}
			if r < 0x10000 {
				b.write_string('\\u')
				mut sh := 12
				for sh >= 0 {
					b.write_byte(lower_hex[u8(r >> sh) & 0xF])
					sh -= 4
				}
			} else {
				b.write_string('\\U')
				mut sh := 28
				for sh >= 0 {
					b.write_byte(lower_hex[u8(r >> sh) & 0xF])
					sh -= 4
				}
			}
		}
	}
}

// append_quoted_with writes a quoted literal for s to b.
fn append_quoted_with(mut b strings.Builder, s string, quote u8, ascii_only bool, graphic_only bool) {
	b.write_byte(quote)
	mut rest := s
	for rest.len > 0 {
		r, width := decode_rune_in_string(rest)
		if width == 1 && r == rune_error {
			// An invalid byte cannot be written as a rune, so it is escaped by
			// its byte value instead.
			b.write_string('\\x')
			b.write_byte(lower_hex[rest[0] >> 4])
			b.write_byte(lower_hex[rest[0] & 0xF])
			rest = rest[1..]
			continue
		}
		append_escaped_rune(mut b, r, quote, ascii_only, graphic_only)
		rest = rest[width..]
	}
	b.write_byte(quote)
}

// append_quoted_rune_with writes a quoted literal for the single rune r to b.
fn append_quoted_rune_with(mut b strings.Builder, r rune, quote u8, ascii_only bool, graphic_only bool) {
	b.write_byte(quote)
	// an out-of-range rune escapes as the replacement character
	rr := if is_valid_rune(r) { r } else { rune_error }
	append_escaped_rune(mut b, rr, quote, ascii_only, graphic_only)
	b.write_byte(quote)
}

// quote_with returns s as a quoted literal using the given quote character.
fn quote_with(s string, quote u8, ascii_only bool, graphic_only bool) string {
	mut b := strings.new_builder(3 * s.len / 2 + 2)
	append_quoted_with(mut b, s, quote, ascii_only, graphic_only)
	return b.str()
}

// quote_rune_with returns r as a quoted literal using the given quote character.
fn quote_rune_with(r rune, quote u8, ascii_only bool, graphic_only bool) string {
	mut b := strings.new_builder(8)
	append_quoted_rune_with(mut b, r, quote, ascii_only, graphic_only)
	return b.str()
}

// builder_to_bytes copies a builder's contents into dst, which is how the
// append_* functions extend a caller-owned buffer.
fn builder_to_bytes(mut b strings.Builder, dst []u8) []u8 {
	mut out := dst.clone()
	out << b.str().bytes()
	return out
}

// quote returns a double-quoted literal representing s, using Go escape
// sequences for control and non-printable characters as decided by is_print.
pub fn quote(s string) string {
	return quote_with(s, 34, false, false)
}

// append_quote appends a double-quoted literal representing s to dst.
pub fn append_quote(dst []u8, s string) []u8 {
	mut b := strings.new_builder(3 * s.len / 2 + 2)
	append_quoted_with(mut b, s, 34, false, false)
	return builder_to_bytes(mut b, dst)
}

// quote_to_ascii returns a double-quoted literal representing s, with every
// non-ASCII character escaped.
pub fn quote_to_ascii(s string) string {
	return quote_with(s, 34, true, false)
}

// append_quote_to_ascii appends the result of quote_to_ascii to dst.
pub fn append_quote_to_ascii(dst []u8, s string) []u8 {
	mut b := strings.new_builder(6 * s.len / 2 + 2)
	append_quoted_with(mut b, s, 34, true, false)
	return builder_to_bytes(mut b, dst)
}

// quote_to_graphic returns a double-quoted literal representing s, escaping
// only the non-graphic characters.
pub fn quote_to_graphic(s string) string {
	return quote_with(s, 34, false, true)
}

// append_quote_to_graphic appends the result of quote_to_graphic to dst.
pub fn append_quote_to_graphic(dst []u8, s string) []u8 {
	mut b := strings.new_builder(6 * s.len / 2 + 2)
	append_quoted_with(mut b, s, 34, false, true)
	return builder_to_bytes(mut b, dst)
}

// quote_rune returns a single-quoted literal representing r.
pub fn quote_rune(r rune) string {
	return quote_rune_with(r, 39, false, false)
}

// append_quote_rune appends a single-quoted literal representing r to dst.
pub fn append_quote_rune(dst []u8, r rune) []u8 {
	mut b := strings.new_builder(8)
	append_quoted_rune_with(mut b, r, 39, false, false)
	return builder_to_bytes(mut b, dst)
}

// quote_rune_to_ascii returns a single-quoted literal representing r, with
// every non-ASCII character escaped.
pub fn quote_rune_to_ascii(r rune) string {
	return quote_rune_with(r, 39, true, false)
}

// append_quote_rune_to_ascii appends the result of quote_rune_to_ascii to dst.
pub fn append_quote_rune_to_ascii(dst []u8, r rune) []u8 {
	mut b := strings.new_builder(12)
	append_quoted_rune_with(mut b, r, 39, true, false)
	return builder_to_bytes(mut b, dst)
}

// quote_rune_to_graphic returns a single-quoted literal representing r, escaping
// only the non-graphic characters.
pub fn quote_rune_to_graphic(r rune) string {
	return quote_rune_with(r, 39, false, true)
}

// append_quote_rune_to_graphic appends the result of quote_rune_to_graphic to dst.
pub fn append_quote_rune_to_graphic(dst []u8, r rune) []u8 {
	mut b := strings.new_builder(12)
	append_quoted_rune_with(mut b, r, 39, false, true)
	return builder_to_bytes(mut b, dst)
}
