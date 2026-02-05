// BEAM rune implementation
// V runes are represented as Erlang integers on BEAM
module builtin

// str converts a rune to string
// Codegen: unicode:characters_to_binary([R])
pub fn (c rune) str() string {
	return ''
}

// string converts a rune array to a string
// Codegen: unicode:characters_to_binary(RA)
pub fn (ra []rune) string() string {
	return ''
}

// repeat returns a new string with count number of copies of the rune
// Codegen: vbeam_string:repeat(unicode:characters_to_binary([R]), Count)
pub fn (c rune) repeat(count int) string {
	return ''
}

// bytes converts a rune to an array of bytes
// Codegen: binary_to_list(unicode:characters_to_binary([R]))
pub fn (c rune) bytes() []u8 {
	return []
}

// length_in_bytes returns the number of bytes needed to store the code point
// Codegen: byte_size(unicode:characters_to_binary([R]))
pub fn (c rune) length_in_bytes() int {
	return 0
}

// to_upper convert to uppercase mode
// Codegen: vbeam_unicode:to_upper(R)
pub fn (c rune) to_upper() rune {
	return c
}

// to_lower convert to lowercase mode
// Codegen: vbeam_unicode:to_lower(R)
pub fn (c rune) to_lower() rune {
	return c
}

// to_title convert to title mode
// Codegen: vbeam_unicode:to_title(R)
pub fn (c rune) to_title() rune {
	return c
}
