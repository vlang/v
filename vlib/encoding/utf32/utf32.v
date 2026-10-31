module utf32

// encode returns the UTF-32 code units of `s` (one u32 per code point).
// Invalid surrogates in `s` are replaced with U+FFFD.
pub fn encode(s string) []u32 {
	mut out := []u32{cap: s.len}
	for r in s.runes() {
		if r > 0x10FFFF || (r >= 0xD800 && r <= 0xDFFF) {
			out << u32(0xFFFD)
		} else {
			out << u32(r)
		}
	}
	return out
}

// decode converts UTF-32 code units to a string. Invalid code points are
// replaced with U+FFFD.
pub fn decode(units []u32) string {
	mut runes := []rune{cap: units.len}
	for u in units {
		if u > 0x10FFFF || (u >= 0xD800 && u <= 0xDFFF) {
			runes << rune(0xFFFD)
		} else {
			runes << rune(u)
		}
	}
	return runes.string()
}

// encode_le returns the little-endian byte representation of `s`.
pub fn encode_le(s string) []u8 {
	mut out := []u8{cap: s.runes().len * 4}
	for u in encode(s) {
		out << u8(u & 0xFF)
		out << u8((u >> 8) & 0xFF)
		out << u8((u >> 16) & 0xFF)
		out << u8((u >> 24) & 0xFF)
	}
	return out
}

// encode_be returns the big-endian byte representation of `s`.
pub fn encode_be(s string) []u8 {
	mut out := []u8{cap: s.runes().len * 4}
	for u in encode(s) {
		out << u8((u >> 24) & 0xFF)
		out << u8((u >> 16) & 0xFF)
		out << u8((u >> 8) & 0xFF)
		out << u8(u & 0xFF)
	}
	return out
}

// decode_le decodes little-endian UTF-32 bytes into a string.
pub fn decode_le(bytes []u8) !string {
	if bytes.len % 4 != 0 {
		return error('utf32: byte length ${bytes.len} is not a multiple of 4')
	}
	mut units := []u32{cap: bytes.len / 4}
	for i := 0; i < bytes.len; i += 4 {
		u := u32(bytes[i]) | (u32(bytes[i + 1]) << 8) | (u32(bytes[i + 2]) << 16) | (u32(bytes[i + 3]) << 24)
		units << u
	}
	return decode(units)
}

// decode_be decodes big-endian UTF-32 bytes into a string.
pub fn decode_be(bytes []u8) !string {
	if bytes.len % 4 != 0 {
		return error('utf32: byte length ${bytes.len} is not a multiple of 4')
	}
	mut units := []u32{cap: bytes.len / 4}
	for i := 0; i < bytes.len; i += 4 {
		u := (u32(bytes[i]) << 24) | (u32(bytes[i + 1]) << 16) | (u32(bytes[i + 2]) << 8) | u32(bytes[i + 3])
		units << u
	}
	return decode(units)
}
