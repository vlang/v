// Module utf16 provides UTF-16 encoding and decoding, as described by the
// Unicode Standard (Chapter 3, "Surrogates").
//
// Code points in the Basic Multilingual Plane are represented by one 16-bit
// code unit. Code points above U+FFFF are represented by a surrogate pair:
// a high surrogate (U+D800..U+DBFF) followed by a low surrogate
// (U+DC00..U+DFFF).
module utf16

const high_surrogate_min = u16(0xD800)
const high_surrogate_max = u16(0xDBFF)
const low_surrogate_min = u16(0xDC00)
const low_surrogate_max = u16(0xDFFF)
const replacement_char = u16(0xFFFD)

// is_surrogate reports whether `r` is a UTF-16 surrogate code point
// (U+D800..U+DFFF). Surrogates are not valid Unicode scalar values, but they
// do appear as individual code units inside UTF-16 data.
pub fn is_surrogate(r rune) bool {
	return r >= 0xD800 && r <= 0xDFFF
}

// is_valid_rune reports whether `r` is a Unicode scalar value, i.e. a code
// point that can be encoded as UTF-16 without loss.
pub fn is_valid_rune(r rune) bool {
	return r <= 0x10FFFF && !is_surrogate(r)
}

// rune_len returns the number of UTF-16 code units needed to represent `r`.
// Valid code points in the BMP need one unit, code points above U+FFFF need
// two. Invalid code points are represented by a single replacement character.
pub fn rune_len(r rune) int {
	if r > 0xFFFF {
		return 2
	}
	return 1
}

// encode_rune returns the UTF-16 code units for `r`.
// Invalid code points and surrogate values are encoded as U+FFFD.
pub fn encode_rune(r rune) []u16 {
	if !is_valid_rune(r) {
		return [replacement_char]
	}
	if r <= 0xFFFF {
		return [u16(r)]
	}
	v := r - 0x10000
	return [u16(0xD800 + (v >> 10)), u16(0xDC00 + (v & 0x3FF))]
}

// append_rune appends the UTF-16 code units for `r` to `units`.
// Invalid code points and surrogate values are appended as U+FFFD.
pub fn append_rune(mut units []u16, r rune) {
	if !is_valid_rune(r) {
		units << replacement_char
		return
	}
	if r <= 0xFFFF {
		units << u16(r)
		return
	}
	v := r - 0x10000
	units << u16(0xD800 + (v >> 10))
	units << u16(0xDC00 + (v & 0x3FF))
}

// encode returns the UTF-16 code units of `s`.
pub fn encode(s string) []u16 {
	mut units := []u16{cap: s.len}
	for r in s.runes() {
		append_rune(mut units, r)
	}
	return units
}

// decode_rune decodes the first code point in `units`.
// It returns the decoded rune and the number of code units consumed.
// An unpaired surrogate or a truncated surrogate pair decodes to U+FFFD and
// consumes one unit.
pub fn decode_rune(units []u16) (rune, int) {
	if units.len == 0 {
		return rune(0xFFFD), 0
	}
	u := units[0]
	if u >= high_surrogate_min && u <= high_surrogate_max {
		if units.len >= 2 {
			u2 := units[1]
			if u2 >= low_surrogate_min && u2 <= low_surrogate_max {
				r := rune(0x10000 + (u32(u - high_surrogate_min) << 10) + u32(u2 - low_surrogate_min))
				return r, 2
			}
		}
		return rune(0xFFFD), 1
	}
	if u >= low_surrogate_min && u <= low_surrogate_max {
		return rune(0xFFFD), 1
	}
	return rune(u), 1
}

// decode converts UTF-16 code units to a string.
// Unpaired surrogates and truncated surrogate pairs are replaced with U+FFFD.
pub fn decode(units []u16) string {
	mut runes := []rune{cap: units.len}
	mut i := 0
	for i < units.len {
		r, n := decode_rune(units[i..])
		runes << r
		i += n
	}
	return runes.string()
}

// decode_strict converts UTF-16 code units to a string, returning an error
// when an unpaired surrogate or a truncated surrogate pair is present.
pub fn decode_strict(units []u16) !string {
	mut runes := []rune{cap: units.len}
	mut i := 0
	for i < units.len {
		u := units[i]
		if u >= high_surrogate_min && u <= high_surrogate_max {
			if i + 1 >= units.len {
				return error('utf16: unpaired high surrogate at index ${i}')
			}
			u2 := units[i + 1]
			if u2 < low_surrogate_min || u2 > low_surrogate_max {
				return error('utf16: high surrogate at index ${i} is not followed by a low surrogate')
			}
			runes << rune(0x10000 + (u32(u - high_surrogate_min) << 10) + u32(u2 - low_surrogate_min))
			i += 2
			continue
		}
		if u >= low_surrogate_min && u <= low_surrogate_max {
			return error('utf16: unpaired low surrogate at index ${i}')
		}
		runes << rune(u)
		i++
	}
	return runes.string()
}

// encode_le returns the little-endian byte representation of `s`.
pub fn encode_le(s string) []u8 {
	return units_to_bytes(encode(s), true)
}

// encode_be returns the big-endian byte representation of `s`.
pub fn encode_be(s string) []u8 {
	return units_to_bytes(encode(s), false)
}

// decode_le decodes little-endian UTF-16 bytes into a string.
// An odd number of bytes is an error; invalid surrogate sequences are
// replaced with U+FFFD.
pub fn decode_le(bytes []u8) !string {
	return decode_bytes(bytes, true)
}

// decode_be decodes big-endian UTF-16 bytes into a string.
// An odd number of bytes is an error; invalid surrogate sequences are
// replaced with U+FFFD.
pub fn decode_be(bytes []u8) !string {
	return decode_bytes(bytes, false)
}

fn units_to_bytes(units []u16, little_endian bool) []u8 {
	mut out := []u8{cap: units.len * 2}
	for u in units {
		if little_endian {
			out << u8(u & 0xFF)
			out << u8(u >> 8)
		} else {
			out << u8(u >> 8)
			out << u8(u & 0xFF)
		}
	}
	return out
}

fn decode_bytes(bytes []u8, little_endian bool) !string {
	if bytes.len % 2 != 0 {
		return error('utf16: odd byte length ${bytes.len}')
	}
	mut units := []u16{cap: bytes.len / 2}
	for i := 0; i < bytes.len; i += 2 {
		u := if little_endian {
			u16(bytes[i]) | (u16(bytes[i + 1]) << 8)
		} else {
			(u16(bytes[i]) << 8) | u16(bytes[i + 1])
		}
		units << u
	}
	return decode(units)
}
