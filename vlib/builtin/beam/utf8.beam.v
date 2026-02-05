// BEAM backend UTF-8 utilities
// Erlang/BEAM has native UTF-8 support in binaries
module builtin

// utf8_str_visible_length returns the visible length of a UTF-8 string
// On BEAM: Erlang binaries are UTF-8 aware, so we can use string:length/1
// Codegen: string:length(S) or unicode:characters_to_list + length
pub fn utf8_str_visible_length(s string) int {
	// Stub - for simple case, return byte length
	// In a proper implementation, this would count grapheme clusters
	return s.len
}

// utf8_char_len returns the length of a UTF-8 character in bytes
pub fn utf8_char_len(b u8) int {
	// Determine UTF-8 byte length from first byte
	if b & 0x80 == 0 {
		return 1
	} else if b & 0xe0 == 0xc0 {
		return 2
	} else if b & 0xf0 == 0xe0 {
		return 3
	} else if b & 0xf8 == 0xf0 {
		return 4
	}
	return 1
}

// utf8_getchar returns the UTF-8 codepoint from a string at position i
// Returns a tuple of (codepoint, byte_length)
pub fn utf8_getchar(s string, index int) (rune, int) {
	// Stub - returns the byte at index as a rune
	// Real implementation would decode UTF-8
	return rune(0), 1
}

// utf32_to_str converts a 32-bit UTF-32 codepoint to a V string
// Codegen: unicode:characters_to_binary([C])
pub fn utf32_to_str(code u32) string {
	return ''
}

// utf32_to_str_no_malloc is like utf32_to_str but avoids heap allocation
// On BEAM, this is the same as utf32_to_str since BEAM handles memory
pub fn utf32_to_str_no_malloc(code u32) string {
	return utf32_to_str(code)
}

// utf32_decode_to_buffer decodes UTF-8 string into a buffer of codepoints
// Returns the number of codepoints decoded
// Codegen: unicode:characters_to_list(S)
fn utf32_decode_to_buffer(s string, buf &u32) int {
	return 0
}
