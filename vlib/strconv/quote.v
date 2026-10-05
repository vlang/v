module strconv

const lower_hex = '0123456789abcdef'

// rune_error is what an invalid UTF-8 sequence decodes to, matching Go's
// utf8.RuneError.
const rune_error = 0xFFFD

// max_rune is the highest valid code point.
const max_rune = 0x10FFFF

// bsearch_u16 returns the index of the first of the n values starting at s
// that is >= v, together with whether that value equals v. The tables are flat
// sorted lists of values, so this is a lower-bound search rather than a range
// search. It takes a pointer and a length rather than an array so that the
// fixed tables in printable_tables.v stay static: slicing one into a dynamic
// array would copy it on every call.
fn bsearch_u16(s &u16, n int, v u16) (int, bool) {
	mut i := 0
	mut j := n
	for i < j {
		h := i + (j - i) >> 1
		if unsafe { s[h] } < v {
			i = h + 1
		} else {
			j = h
		}
	}
	return i, i < n && unsafe { s[i] } == v
}

// bsearch_u32 is bsearch_u16 for the 32 bit table.
fn bsearch_u32(s &u32, n int, v u32) (int, bool) {
	mut i := 0
	mut j := n
	for i < j {
		h := i + (j - i) >> 1
		if unsafe { s[h] } < v {
			i = h + 1
		} else {
			j = h
		}
	}
	return i, i < n && unsafe { s[i] } == v
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
		i, _ := bsearch_u16(&is_print16[0], is_print16.len, rr)
		if i >= is_print16.len || rr < is_print16[i & ~1] || is_print16[i | 1] < rr {
			return false
		}
		_, found := bsearch_u16(&is_not_print16[0], is_not_print16.len, rr)
		return !found
	}

	rr := u32(r)
	i, _ := bsearch_u32(&is_print32[0], is_print32.len, rr)
	if i >= is_print32.len || rr < is_print32[i & ~1] || is_print32[i | 1] < rr {
		return false
	}
	if r >= 0x20000 {
		return true
	}
	_, found := bsearch_u16(&is_not_print32[0], is_not_print32.len, u16(r - 0x10000))
	return !found
}

// is_in_graphic_list reports whether r is in is_graphic_list. Kept separate
// from is_graphic so that quoting can skip a second is_print call.
fn is_in_graphic_list(r rune) bool {
	// the list only covers the basic multilingual plane
	if r > 0xFFFF {
		return false
	}
	_, found := bsearch_u16(&is_graphic_list[0], is_graphic_list.len, u16(r))
	return found
}

// is_graphic reports whether r is defined as graphic by Unicode: letters,
// marks, numbers, punctuation, symbols and spaces, from categories L, M, N, P,
// S and Zs. It differs from is_print only in also accepting the Zs spaces
// other than U+0020, such as U+00A0 and U+3000.
pub fn is_graphic(r rune) bool {
	if is_print(r) {
		return true
	}
	return is_in_graphic_list(r)
}

// decode_rune_at decodes the rune that starts at byte i of s, returning it and
// the number of bytes it occupies, like Go's utf8.DecodeRuneInString(s[i:]).
// An invalid sequence decodes to rune_error with a width of 1, so a caller
// walking a string always makes progress. It indexes s rather than taking a
// slice of it, because a V slice is a copy.
fn decode_rune_at(s string, i int) (rune, int) {
	n := s.len - i
	if n <= 0 {
		return rune_error, 0
	}
	b0 := s[i]
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
	if n < sz {
		return rune_error, 1
	}
	b1 := s[i + 1]
	if b1 < lo || b1 > hi {
		return rune_error, 1
	}
	mut r := rune(b0 & (0x7F >> sz)) << 6 | rune(b1 & 0x3F)
	if sz >= 3 {
		b2 := s[i + 2]
		if b2 < 0x80 || b2 > 0xBF {
			return rune_error, 1
		}
		r = rune(u32(r) << 6) | rune(b2 & 0x3F)
	}
	if sz == 4 {
		b3 := s[i + 3]
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

// can_backquote reports whether s can be written unchanged as a Go raw string
// literal, between backquotes: it holds no control character other than tab,
// no backquote, no DEL, no invalid UTF-8 and no byte order mark.
pub fn can_backquote(s string) bool {
	mut i := 0
	for i < s.len {
		r, width := decode_rune_at(s, i)
		i += width
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

// append_escape appends a backslash followed by c to buf.
@[inline]
fn append_escape(mut buf []u8, c u8) {
	buf << 0x5C
	buf << c
}

// append_escaped_rune appends r to buf, escaping it if it cannot appear literally.
fn append_escaped_rune(mut buf []u8, r rune, quote u8, ascii_only bool, graphic_only bool) {
	if r == rune(quote) || r == 0x5C {
		// the quote character and the backslash are always escaped
		append_escape(mut buf, u8(r))
		return
	}
	if ascii_only {
		if r < 0x80 && is_print(r) {
			buf << u8(r)
			return
		}
	} else if is_print(r) || (graphic_only && is_in_graphic_list(r)) {
		append_rune(mut buf, r)
		return
	}
	match r {
		0x07 {
			append_escape(mut buf, `a`)
		}
		0x08 {
			append_escape(mut buf, `b`)
		}
		0x0C {
			append_escape(mut buf, `f`)
		}
		0x0A {
			append_escape(mut buf, `n`)
		}
		0x0D {
			append_escape(mut buf, `r`)
		}
		0x09 {
			append_escape(mut buf, `t`)
		}
		0x0B {
			append_escape(mut buf, `v`)
		}
		else {
			if r < 0x20 || r == 0x7F {
				append_escape(mut buf, `x`)
				buf << lower_hex[u8(r) >> 4]
				buf << lower_hex[u8(r) & 0xF]
				return
			}
			if !is_valid_rune(r) {
				// an out-of-range rune escapes as the replacement character
				append_escape(mut buf, `u`)
				buf << `f`
				buf << `f`
				buf << `f`
				buf << `d`
				return
			}
			mut sh := 0
			if r < 0x10000 {
				append_escape(mut buf, `u`)
				sh = 12
			} else {
				append_escape(mut buf, `U`)
				sh = 28
			}
			for sh >= 0 {
				buf << lower_hex[u8(r >> sh) & 0xF]
				sh -= 4
			}
		}
	}
}

// append_quoted_with appends a quoted literal for s to buf.
fn append_quoted_with(mut buf []u8, s string, quote u8, ascii_only bool, graphic_only bool) {
	buf << quote
	mut i := 0
	for i < s.len {
		r, width := decode_rune_at(s, i)
		if width == 1 && r == rune_error {
			// An invalid byte cannot be written as a rune, so it is escaped by
			// its byte value instead.
			append_escape(mut buf, `x`)
			buf << lower_hex[s[i] >> 4]
			buf << lower_hex[s[i] & 0xF]
		} else {
			append_escaped_rune(mut buf, r, quote, ascii_only, graphic_only)
		}
		i += width
	}
	buf << quote
}

// append_quoted_rune_with appends a quoted literal for the single rune r to buf.
fn append_quoted_rune_with(mut buf []u8, r rune, quote u8, ascii_only bool, graphic_only bool) {
	buf << quote
	// an out-of-range rune escapes as the replacement character
	rr := if is_valid_rune(r) { r } else { rune_error }
	append_escaped_rune(mut buf, rr, quote, ascii_only, graphic_only)
	buf << quote
}

// quote_with returns s as a quoted literal using the given quote character.
fn quote_with(s string, quote u8, ascii_only bool, graphic_only bool) string {
	mut buf := []u8{cap: 3 * s.len / 2 + 2}
	append_quoted_with(mut buf, s, quote, ascii_only, graphic_only)
	return buf.bytestr()
}

// quote_rune_with returns r as a quoted literal using the given quote character.
fn quote_rune_with(r rune, quote u8, ascii_only bool, graphic_only bool) string {
	mut buf := []u8{cap: 12}
	append_quoted_rune_with(mut buf, r, quote, ascii_only, graphic_only)
	return buf.bytestr()
}

// quote returns a double-quoted Go string literal representing s, using Go
// escape sequences for control characters and the runes is_print rejects.
// Note that `$` is not escaped, so the result is not always a valid V literal.
pub fn quote(s string) string {
	return quote_with(s, `"`, false, false)
}

// append_quote appends the result of quote(s) to dst.
pub fn append_quote(mut dst []u8, s string) {
	append_quoted_with(mut dst, s, `"`, false, false)
}

// quote_to_ascii returns a double-quoted Go string literal representing s,
// with every non-ASCII character escaped.
pub fn quote_to_ascii(s string) string {
	return quote_with(s, `"`, true, false)
}

// append_quote_to_ascii appends the result of quote_to_ascii(s) to dst.
pub fn append_quote_to_ascii(mut dst []u8, s string) {
	append_quoted_with(mut dst, s, `"`, true, false)
}

// quote_to_graphic returns a double-quoted Go string literal representing s,
// escaping only the characters is_graphic rejects.
pub fn quote_to_graphic(s string) string {
	return quote_with(s, `"`, false, true)
}

// append_quote_to_graphic appends the result of quote_to_graphic(s) to dst.
pub fn append_quote_to_graphic(mut dst []u8, s string) {
	append_quoted_with(mut dst, s, `"`, false, true)
}

// quote_rune returns a single-quoted Go rune literal representing r. An
// invalid code point is quoted as U+FFFD.
pub fn quote_rune(r rune) string {
	return quote_rune_with(r, `'`, false, false)
}

// append_quote_rune appends the result of quote_rune(r) to dst.
pub fn append_quote_rune(mut dst []u8, r rune) {
	append_quoted_rune_with(mut dst, r, `'`, false, false)
}

// quote_rune_to_ascii returns a single-quoted Go rune literal representing r,
// escaping it if it is not ASCII.
pub fn quote_rune_to_ascii(r rune) string {
	return quote_rune_with(r, `'`, true, false)
}

// append_quote_rune_to_ascii appends the result of quote_rune_to_ascii(r) to dst.
pub fn append_quote_rune_to_ascii(mut dst []u8, r rune) {
	append_quoted_rune_with(mut dst, r, `'`, true, false)
}

// quote_rune_to_graphic returns a single-quoted Go rune literal representing
// r, escaping it if is_graphic rejects it.
pub fn quote_rune_to_graphic(r rune) string {
	return quote_rune_with(r, `'`, false, true)
}

// append_quote_rune_to_graphic appends the result of quote_rune_to_graphic(r)
// to dst.
pub fn append_quote_rune_to_graphic(mut dst []u8, r rune) {
	append_quoted_rune_with(mut dst, r, `'`, false, true)
}
