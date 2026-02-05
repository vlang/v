// BEAM string implementation
// V strings are represented as Erlang binaries on BEAM
module builtin

// string is the V string type
// On BEAM: represented as Erlang binary <<...>>
pub struct string {
pub:
	// On BEAM, these fields exist for V type checker compatibility
	// The actual representation is an Erlang binary, not a C-style struct
	// str is a dummy field to satisfy code that accesses s.str (like strconv)
	// In reality, BEAM strings don't have separate pointer/length
	str &u8 = unsafe { nil }
	len int
}

// str returns the string itself
pub fn (s string) str() string {
	return s
}

// len returns the string length
// Codegen: byte_size(S)
pub fn (s string) len() int {
	return s.len
}

// bytes returns the string as bytes
// Codegen: binary_to_list(S)
pub fn (s string) bytes() []u8 {
	return []
}

// runes returns an array of all the utf runes in the string
// Codegen: unicode:characters_to_list(S)
pub fn (s string) runes() []rune {
	return []
}

// repeat returns the string repeated n times
// Codegen: vbeam_string:repeat(S, N)
pub fn (s string) repeat(count int) string {
	return ''
}

// contains checks if the string contains another
// Codegen: vbeam_string:contains(S, Sub)
pub fn (s string) contains(substr string) bool {
	return false
}

// index returns the position of substr or none
// Codegen: vbeam_string:index(S, Sub)
pub fn (s string) index(p string) ?int {
	idx := s.index_(p)
	if idx == -1 {
		return none
	}
	return idx
}

// index_ returns the position of the first character of the input string.
// It will return `-1` if the input string can't be found.
// Codegen: vbeam_string:index(S, P)
pub fn (s string) index_(p string) int {
	return -1
}

// split splits the string by delimiter
// Codegen: vbeam_string:split(S, Delim)
pub fn (s string) split(delim string) []string {
	return []
}

// trim removes specified characters from both ends
// Codegen: vbeam_string:trim(S, Cutset)
pub fn (s string) trim(cutset string) string {
	return ''
}

// trim_space removes whitespace from both ends
// Codegen: vbeam_string:trim(S)
pub fn (s string) trim_space() string {
	return ''
}

// to_upper converts to uppercase
// Codegen: string:uppercase(S)
pub fn (s string) to_upper() string {
	return ''
}

// to_lower converts to lowercase
// Codegen: string:lowercase(S)
pub fn (s string) to_lower() string {
	return ''
}

// starts_with checks if string starts with prefix
// Codegen: vbeam_string:starts_with(S, Prefix)
pub fn (s string) starts_with(prefix string) bool {
	return false
}

// ends_with checks if string ends with suffix
// Codegen: vbeam_string:ends_with(S, Suffix)
pub fn (s string) ends_with(suffix string) bool {
	return false
}

// replace replaces occurrences
// Codegen: vbeam_string:replace(S, Old, New)
pub fn (s string) replace(old string, new_ string) string {
	return ''
}

// substr returns a substring
// Codegen: vbeam_string:slice(S, Start, End)
pub fn (s string) substr(start int, end int) string {
	return ''
}

// substr_unsafe returns a substring without bounds checking
// Codegen: vbeam_string:slice(S, Start, End)
// On BEAM: same as substr since BEAM handles bounds internally
@[unsafe]
pub fn (s string) substr_unsafe(start int, end int) string {
	return ''
}

// int parses the string as an integer
// Codegen: vbeam_conv:string_to_int(S)
pub fn (s string) int() int {
	return 0
}

// i8 parses the string as an 8-bit integer
// Codegen: vbeam_conv:string_to_int(S) band 16#FF
pub fn (s string) i8() i8 {
	return 0
}

// i16 parses the string as a 16-bit integer
// Codegen: vbeam_conv:string_to_int(S) band 16#FFFF
pub fn (s string) i16() i16 {
	return 0
}

// i32 parses the string as a 32-bit integer
// Codegen: vbeam_conv:string_to_int(S) band 16#FFFFFFFF
pub fn (s string) i32() i32 {
	return 0
}

// i64 parses the string as a 64-bit integer
// Codegen: vbeam_conv:string_to_int(S)
pub fn (s string) i64() i64 {
	return 0
}

// u8 parses the string as an unsigned 8-bit integer
// Codegen: vbeam_conv:string_to_int(S) band 16#FF
pub fn (s string) u8() u8 {
	return 0
}

// u16 parses the string as an unsigned 16-bit integer
// Codegen: vbeam_conv:string_to_int(S) band 16#FFFF
pub fn (s string) u16() u16 {
	return 0
}

// u32 parses the string as an unsigned 32-bit integer
// Codegen: vbeam_conv:string_to_int(S) band 16#FFFFFFFF
pub fn (s string) u32() u32 {
	return 0
}

// u64 parses the string as an unsigned 64-bit integer
// Codegen: vbeam_conv:string_to_int(S)
pub fn (s string) u64() u64 {
	return 0
}

// f64 parses the string as a float
// Codegen: vbeam_conv:string_to_float(S)
pub fn (s string) f64() f64 {
	return 0.0
}

// f32 parses the string as a 32-bit float
// Codegen: vbeam_conv:string_to_float(S)
pub fn (s string) f32() f32 {
	return 0.0
}

// == equality operator (handled by codegen)
pub fn (s string) == (other string) bool {
	return false
}

// + concatenation operator
// Codegen: <<S/binary, Other/binary>>
pub fn (s string) + (other string) string {
	return ''
}

// contains_only returns true if string contains only chars from chars
pub fn (s string) contains_only(chars string) bool {
	// Beam backend stub
	return false
}

// is_capital returns true if the first char is uppercase followed by lowercase
pub fn (s string) is_capital() bool {
	// Beam backend stub
	return false
}

// all_before returns the string before the first occurrence of sub
pub fn (s string) all_before(sub string) string {
	return ''
}

// all_after returns the string after the first occurrence of sub
pub fn (s string) all_after(sub string) string {
	return ''
}

// all_after_last returns the string after the last occurrence of sub
pub fn (s string) all_after_last(sub string) string {
	return ''
}

// all_before_last returns the string before the last occurrence of sub
pub fn (s string) all_before_last(sub string) string {
	return ''
}

// last_index_u8 returns the position of the last occurrence of byte c, or -1
// Codegen: vbeam_string:last_index_u8(S, C)
pub fn (s string) last_index_u8(c u8) int {
	return -1
}

// last_index returns the position of the last occurrence of needle, or none
// Codegen: vbeam_string:last_index(S, Needle)
pub fn (s string) last_index(needle string) ?int {
	// Stub implementation - would return position or none
	return none
}

// index_last_ returns the position of the last occurrence of needle, or -1
fn (s string) index_last_(needle string) int {
	return -1
}

// trim_space_right removes whitespace from the right end
// Codegen: vbeam_string:trim_right(S)
pub fn (s string) trim_space_right() string {
	return s
}

// capitalize returns string with first character capitalized
// Codegen: vbeam_string:capitalize(S)
pub fn (s string) capitalize() string {
	return s
}

// split_into_lines splits the string into lines
// Codegen: vbeam_string:split_into_lines(S)
pub fn (s string) split_into_lines() []string {
	return []string{}
}

// clone returns a copy of the string
// On BEAM, binaries are immutable, so clone just returns the same reference
pub fn (s string) clone() string {
	return s
}

// trim_right removes trailing characters from the string
// Codegen: vbeam_string:trim_right(S, Chars)
pub fn (s string) trim_right(chars string) string {
	return s
}

// trim_left removes leading characters from the string
// Codegen: vbeam_string:trim_left(S, Chars)
pub fn (s string) trim_left(chars string) string {
	return s
}

// index_u8 returns the position of the first occurrence of byte c, or -1
// Codegen: vbeam_string:index_u8(S, C)
pub fn (s string) index_u8(c u8) int {
	return -1
}

// free is a no-op on BEAM (binaries are garbage collected)
@[unsafe]
pub fn (s &string) free() {
	// No-op on BEAM
}

// replace_once replaces the first occurrence
// Codegen: vbeam_string:replace_once(S, Old, New)
pub fn (s string) replace_once(old string, new_ string) string {
	return s
}

// count returns the number of non-overlapping occurrences of substr in the string
// Codegen: vbeam_string:count(S, Sub)
pub fn (s string) count(substr string) int {
	return 0
}

// replace_each replaces all occurrences of the string pairs given in `vals`.
// Example: assert 'ABCD'.replace_each(['B','C/','C','D','D','C']) == 'AC/DC'
// Codegen: vbeam_string:replace_each(S, Vals)
pub fn (s string) replace_each(vals []string) string {
	if s.len == 0 || vals.len == 0 {
		return s.clone()
	}
	// Beam backend stub - would iterate over pairs and replace
	return s
}

// index_after returns the position of the input string, starting search from `start` position.
// Codegen: vbeam_string:index_after(S, P, Start)
pub fn (s string) index_after(p string, start int) ?int {
	idx := s.index_after_(p, start)
	if idx == -1 {
		return none
	}
	return idx
}

// index_after_ returns the position of the input string, starting search from `start` position.
// Returns -1 if not found.
// Codegen: vbeam_string:index_after(S, P, Start)
pub fn (s string) index_after_(p string, start int) int {
	if p.len > s.len {
		return -1
	}
	// Beam backend stub
	return -1
}

// index_any returns the position of any of the characters in the input string - if found.
// Codegen: vbeam_string:index_any(S, Chars)
pub fn (s string) index_any(chars string) int {
	// Beam backend stub
	return -1
}

// trim_string_left removes a prefix string if present
// Codegen: vbeam_string:trim_string_left(S, Prefix)
pub fn (s string) trim_string_left(str string) string {
	return s
}

// trim_string_right removes a suffix string if present
// Codegen: vbeam_string:trim_string_right(S, Suffix)
pub fn (s string) trim_string_right(str string) string {
	return s
}

// cstring_to_vstring creates a new V string copy of the C style string.
// On BEAM: C strings don't exist in the same way, this converts from
// what would be the BEAM equivalent (likely a binary or charlist)
// It will panic, if the pointer `s` is 0.
@[unsafe]
pub fn cstring_to_vstring(const_s &char) string {
	// On BEAM, this would typically receive an Erlang binary or charlist
	// and convert it to a V string (which is also a binary on BEAM)
	// Stub: returns empty string, actual codegen will handle this
	return ''
}

// tos_clone creates a new V string copy of the C style string.
// See also cstring_to_vstring.
@[unsafe]
pub fn tos_clone(const_s &u8) string {
	return ''
}

// tos creates a string from a byte pointer and length (unsafe)
@[unsafe]
pub fn tos(s &u8, len int) string {
	return ''
}

// tos2 creates a string from a byte pointer (unsafe, null-terminated)
@[unsafe]
pub fn tos2(s &u8) string {
	return ''
}

// tos3 creates a string from a char pointer (unsafe, null-terminated)
@[unsafe]
pub fn tos3(s &char) string {
	return ''
}

// tos4 creates a string from a byte pointer (unsafe, null-terminated)
@[unsafe]
pub fn tos4(s &u8) string {
	return ''
}

// tos5 creates a string from a char pointer (unsafe, null-terminated)
@[unsafe]
pub fn tos5(s &char) string {
	return ''
}

// contains_any returns true if the string contains any of the chars
// Codegen: lists:any(fun(C) -> binary:match(S, C) /= nomatch end, binary_to_list(Chars))
pub fn (s string) contains_any(chars string) bool {
	return false
}

// bool parses the string as a boolean
pub fn (s string) bool() bool {
	return s == 'true' || s == '1'
}

// match_glob matches the string against a glob pattern
pub fn (s string) match_glob(pattern string) bool {
	return false
}

// split_once splits the string at the first occurrence of delim
// Returns (before, after) tuple, or none if delim not found
// Codegen: vbeam_string:split_once(S, Delim)
pub fn (s string) split_once(delim string) ?(string, string) {
	idx := s.index_(delim)
	if idx == -1 {
		return none
	}
	return s.substr(0, idx), s.substr(idx + delim.len, s.len)
}

// split_nth splits the string by delim into at most n parts
// Codegen: vbeam_string:split_nth(S, Delim, N)
pub fn (s string) split_nth(delim string, n int) []string {
	return []
}
