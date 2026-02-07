// BEAM rune implementation
// V runes are represented as Erlang integers on BEAM
module builtin

// str converts a rune to string
// Encodes the Unicode codepoint as a UTF-8 binary string
pub fn (c rune) str() string {
	// Build the UTF-8 encoded bytes
	encoded := c.bytes()
	mut result := ''
	for b in encoded {
		result += b.ascii_str()
	}
	return result
}

// string converts a rune array to a string
// Encodes each rune as UTF-8 and concatenates
pub fn (ra []rune) string() string {
	mut result := ''
	for c in ra {
		result += c.str()
	}
	return result
}

// repeat returns a new string with count number of copies of the rune
pub fn (c rune) repeat(count int) string {
	if count <= 0 {
		return ''
	}
	s := c.str()
	mut result := ''
	for _ in 0 .. count {
		result += s
	}
	return result
}

// bytes converts a rune to an array of UTF-8 bytes
pub fn (c rune) bytes() []u8 {
	mut buf := []u8{}
	cp := int(c)
	if cp < 0x80 {
		// 1-byte: 0xxxxxxx
		buf << u8(cp)
	} else if cp < 0x800 {
		// 2-byte: 110xxxxx 10xxxxxx
		buf << u8(0xC0 | (cp >> 6))
		buf << u8(0x80 | (cp & 0x3F))
	} else if cp < 0x10000 {
		// 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
		buf << u8(0xE0 | (cp >> 12))
		buf << u8(0x80 | ((cp >> 6) & 0x3F))
		buf << u8(0x80 | (cp & 0x3F))
	} else {
		// 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
		buf << u8(0xF0 | (cp >> 18))
		buf << u8(0x80 | ((cp >> 12) & 0x3F))
		buf << u8(0x80 | ((cp >> 6) & 0x3F))
		buf << u8(0x80 | (cp & 0x3F))
	}
	return buf
}

// length_in_bytes returns the number of bytes needed to store the code point in UTF-8
pub fn (c rune) length_in_bytes() int {
	cp := int(c)
	if cp < 0x80 {
		return 1
	} else if cp < 0x800 {
		return 2
	} else if cp < 0x10000 {
		return 3
	}
	return 4
}

// to_upper convert to uppercase mode
// Codegen: string:uppercase handled by codegen for method calls
pub fn (c rune) to_upper() rune {
	// ASCII fast path
	if c >= `a` && c <= `z` {
		return rune(int(c) - 32)
	}
	return c
}

// to_lower convert to lowercase mode
// Codegen: string:lowercase handled by codegen for method calls
pub fn (c rune) to_lower() rune {
	// ASCII fast path
	if c >= `A` && c <= `Z` {
		return rune(int(c) + 32)
	}
	return c
}

// to_title convert to title mode
// For ASCII, title case is the same as uppercase
pub fn (c rune) to_title() rune {
	return c.to_upper()
}
