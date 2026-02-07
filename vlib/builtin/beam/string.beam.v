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

// byte_in_str checks if byte c is present in the string chars.
// Uses contains() which codegen maps to binary:match.
fn byte_in_str(c u8, chars string) bool {
	return chars.contains(c.ascii_str())
}

// str returns the string itself
pub fn (s string) str() string {
	return s
}

// len returns the string length
// Codegen handles: byte_size(S)
pub fn (s string) len() int {
	return s.len
}

// bytes returns the string as bytes
// Codegen intercepts → erlang:binary_to_list(S)
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) bytes() []u8 {
	return []
}

// runes returns an array of all the utf runes in the string
// Codegen intercepts → unicode:characters_to_list(S)
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) runes() []rune {
	return []
}

// repeat returns the string repeated n times
// Not handled by codegen — real V implementation
pub fn (s string) repeat(count int) string {
	if count <= 0 || s.len == 0 {
		return ''
	}
	mut result := ''
	for _ in 0 .. count {
		result = result + s
	}
	return result
}

// contains checks if the string contains another
// Codegen intercepts → binary:match(S, Sub) case → true/false
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) contains(substr string) bool {
	return false
}

// index returns the position of substr or none
pub fn (s string) index(p string) ?int {
	idx := s.index_(p)
	if idx == -1 {
		return none
	}
	return idx
}

// index_ returns the position of the first character of the input string.
// It will return `-1` if the input string can't be found.
// Not handled by codegen — real V implementation
pub fn (s string) index_(p string) int {
	if p.len == 0 {
		return 0
	}
	if p.len > s.len {
		return -1
	}
	for i := 0; i <= s.len - p.len; i++ {
		if s.substr(i, i + p.len) == p {
			return i
		}
	}
	return -1
}

// split splits the string by delimiter
// Codegen intercepts → binary:split(S, Delim, [global])
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) split(delim string) []string {
	return []
}

// trim removes specified characters from both ends
// Not handled by codegen (the no-arg variant is handled as trim_space)
pub fn (s string) trim(cutset string) string {
	return s.trim_left(cutset).trim_right(cutset)
}

// trim_space removes whitespace from both ends
// Codegen intercepts → string:trim(S)
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) trim_space() string {
	return ''
}

// to_upper converts to uppercase
// Codegen intercepts → string:uppercase(S)
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) to_upper() string {
	return ''
}

// to_lower converts to lowercase
// Codegen intercepts → string:lowercase(S)
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) to_lower() string {
	return ''
}

// starts_with checks if string starts with prefix
// Codegen intercepts → string:prefix(S, Prefix) != nomatch
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) starts_with(prefix string) bool {
	return false
}

// ends_with checks if string ends with suffix
// Codegen intercepts → binary:longest_common_suffix check
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) ends_with(suffix string) bool {
	return false
}

// replace replaces all occurrences of old with new
// Codegen intercepts → binary:replace(S, Old, New, [global])
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) replace(old string, new_ string) string {
	return ''
}

// substr returns a substring
// Not handled by codegen — uses V slice syntax which codegen translates to binary_part
pub fn (s string) substr(start int, end int) string {
	if start >= end || start >= s.len {
		return ''
	}
	mut actual_end := end
	if actual_end > s.len {
		actual_end = s.len
	}
	return s[start..actual_end]
}

// substr_unsafe returns a substring without bounds checking
// On BEAM: same as substr since BEAM handles bounds internally
@[unsafe]
pub fn (s string) substr_unsafe(start int, end int) string {
	return s.substr(start, end)
}

// int parses the string as an integer
// Codegen intercepts → erlang:binary_to_integer(S)
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) int() int {
	return 0
}

// i8 parses the string as an 8-bit integer
pub fn (s string) i8() i8 {
	return i8(s.int())
}

// i16 parses the string as a 16-bit integer
pub fn (s string) i16() i16 {
	return i16(s.int())
}

// i32 parses the string as a 32-bit integer
pub fn (s string) i32() i32 {
	return i32(s.int())
}

// i64 parses the string as a 64-bit integer
// Codegen intercepts → erlang:binary_to_integer(S)
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) i64() i64 {
	return 0
}

// u8 parses the string as an unsigned 8-bit integer
pub fn (s string) u8() u8 {
	return u8(s.int())
}

// u16 parses the string as an unsigned 16-bit integer
pub fn (s string) u16() u16 {
	return u16(s.int())
}

// u32 parses the string as an unsigned 32-bit integer
pub fn (s string) u32() u32 {
	return u32(s.int())
}

// u64 parses the string as an unsigned 64-bit integer
pub fn (s string) u64() u64 {
	return u64(s.int())
}

// f64 parses the string as a float
// Codegen intercepts → erlang:binary_to_float(S)
// Stub body is dead code — codegen emits Erlang BIF directly
pub fn (s string) f64() f64 {
	return 0.0
}

// f32 parses the string as a 32-bit float
pub fn (s string) f32() f32 {
	return f32(s.f64())
}

// == equality operator
// Codegen intercepts → Erlang =:= operator
// Stub body is dead code — codegen emits comparison directly
pub fn (s string) == (other string) bool {
	return false
}

// + concatenation operator
// Codegen intercepts → <<S/binary, Other/binary>>
// Stub body is dead code — codegen emits binary concatenation directly
pub fn (s string) + (other string) string {
	return ''
}

// contains_only returns true if string contains only chars from chars
// Not handled by codegen — real V implementation
pub fn (s string) contains_only(chars string) bool {
	if s.len == 0 {
		return true
	}
	s_bytes := s.bytes()
	for b in s_bytes {
		if !byte_in_str(b, chars) {
			return false
		}
	}
	return true
}

// is_capital returns true if the first char is uppercase followed by no uppercase
// Not handled by codegen — real V implementation
pub fn (s string) is_capital() bool {
	if s.len == 0 {
		return false
	}
	b := s.bytes()
	// First char must be A-Z (65-90)
	if b[0] < 65 || b[0] > 90 {
		return false
	}
	// Remaining chars must not be A-Z
	for i := 1; i < s.len; i++ {
		if b[i] >= 65 && b[i] <= 90 {
			return false
		}
	}
	return true
}

// all_before returns the string before the first occurrence of sub
// Not handled by codegen — real V implementation
pub fn (s string) all_before(sub string) string {
	idx := s.index_(sub)
	if idx == -1 {
		return s
	}
	return s.substr(0, idx)
}

// all_after returns the string after the first occurrence of sub
// Not handled by codegen — real V implementation
pub fn (s string) all_after(sub string) string {
	idx := s.index_(sub)
	if idx == -1 {
		return s
	}
	return s.substr(idx + sub.len, s.len)
}

// all_after_last returns the string after the last occurrence of sub
// Not handled by codegen — real V implementation
pub fn (s string) all_after_last(sub string) string {
	idx := s.index_last_(sub)
	if idx == -1 {
		return s
	}
	return s.substr(idx + sub.len, s.len)
}

// all_before_last returns the string before the last occurrence of sub
// Not handled by codegen — real V implementation
pub fn (s string) all_before_last(sub string) string {
	idx := s.index_last_(sub)
	if idx == -1 {
		return s
	}
	return s.substr(0, idx)
}

// last_index_u8 returns the position of the last occurrence of byte c, or -1
// Not handled by codegen — real V implementation
pub fn (s string) last_index_u8(c u8) int {
	b := s.bytes()
	for i := s.len - 1; i >= 0; i-- {
		if b[i] == c {
			return i
		}
	}
	return -1
}

// last_index returns the position of the last occurrence of needle, or none
pub fn (s string) last_index(needle string) ?int {
	idx := s.index_last_(needle)
	if idx == -1 {
		return none
	}
	return idx
}

// index_last_ returns the position of the last occurrence of needle, or -1
// Not handled by codegen — real V implementation
fn (s string) index_last_(needle string) int {
	if needle.len == 0 {
		return s.len
	}
	if needle.len > s.len {
		return -1
	}
	for i := s.len - needle.len; i >= 0; i-- {
		if s.substr(i, i + needle.len) == needle {
			return i
		}
	}
	return -1
}

// trim_space_right removes whitespace from the right end
// Not handled by codegen — real V implementation
pub fn (s string) trim_space_right() string {
	return s.trim_right(' \t\n\r')
}

// capitalize returns string with first character capitalized
// Not handled by codegen — real V implementation
pub fn (s string) capitalize() string {
	if s.len == 0 {
		return ''
	}
	first := s.substr(0, 1).to_upper()
	if s.len == 1 {
		return first
	}
	rest := s.substr(1, s.len).to_lower()
	return first + rest
}

// split_into_lines splits the string into lines
// Codegen handles: binary:split(S, "\n", [global])
pub fn (s string) split_into_lines() []string {
	return []string{}
}

// clone returns a copy of the string
// On BEAM, binaries are immutable, so clone just returns the same reference
pub fn (s string) clone() string {
	return s
}

// trim_right removes trailing characters from the string
// Not handled by codegen — real V implementation
// Uses byte_in_str helper to avoid mut-in-nested-loop pattern
pub fn (s string) trim_right(chars string) string {
	if s.len == 0 || chars.len == 0 {
		return s
	}
	s_bytes := s.bytes()
	mut end := s.len
	for end > 0 {
		if !byte_in_str(s_bytes[end - 1], chars) {
			break
		}
		end--
	}
	if end == s.len {
		return s
	}
	if end == 0 {
		return ''
	}
	return s.substr(0, end)
}

// trim_left removes leading characters from the string
// Not handled by codegen — real V implementation
// Uses byte_in_str helper to avoid mut-in-nested-loop pattern
pub fn (s string) trim_left(chars string) string {
	if s.len == 0 || chars.len == 0 {
		return s
	}
	s_bytes := s.bytes()
	mut start := 0
	for start < s.len {
		if !byte_in_str(s_bytes[start], chars) {
			break
		}
		start++
	}
	if start == 0 {
		return s
	}
	if start >= s.len {
		return ''
	}
	return s.substr(start, s.len)
}

// index_u8 returns the position of the first occurrence of byte c, or -1
// Not handled by codegen — real V implementation
pub fn (s string) index_u8(c u8) int {
	b := s.bytes()
	for i := 0; i < s.len; i++ {
		if b[i] == c {
			return i
		}
	}
	return -1
}

// free is a no-op on BEAM (binaries are garbage collected)
@[unsafe]
pub fn (s &string) free() {
	// No-op on BEAM
}

// replace_once replaces the first occurrence
// Not handled by codegen — real V implementation
pub fn (s string) replace_once(old string, new_ string) string {
	idx := s.index_(old)
	if idx == -1 {
		return s
	}
	before := s.substr(0, idx)
	after := s.substr(idx + old.len, s.len)
	return before + new_ + after
}

// count returns the number of non-overlapping occurrences of substr in the string
// Not handled by codegen — real V implementation
pub fn (s string) count(substr string) int {
	if substr.len == 0 || substr.len > s.len {
		return 0
	}
	parts := s.split(substr)
	return parts.len - 1
}

// replace_each replaces all occurrences of the string pairs given in `vals`.
// Not handled by codegen — real V implementation
pub fn (s string) replace_each(vals []string) string {
	if s.len == 0 || vals.len == 0 {
		return s.clone()
	}
	mut result := s
	mut i := 0
	for i + 1 < vals.len {
		result = result.replace(vals[i], vals[i + 1])
		i += 2
	}
	return result
}

// index_after returns the position of the input string, starting search from `start` position.
pub fn (s string) index_after(p string, start int) ?int {
	idx := s.index_after_(p, start)
	if idx == -1 {
		return none
	}
	return idx
}

// index_after_ returns the position of the input string, starting search from `start` position.
// Not handled by codegen — real V implementation
pub fn (s string) index_after_(p string, start int) int {
	if p.len > s.len || start >= s.len {
		return -1
	}
	remainder := s.substr(start, s.len)
	idx := remainder.index_(p)
	if idx == -1 {
		return -1
	}
	return start + idx
}

// index_any returns the position of any of the characters in the input string - if found.
// Not handled by codegen — real V implementation using byte_in_str helper
pub fn (s string) index_any(chars string) int {
	if s.len == 0 || chars.len == 0 {
		return -1
	}
	s_bytes := s.bytes()
	for i := 0; i < s.len; i++ {
		if byte_in_str(s_bytes[i], chars) {
			return i
		}
	}
	return -1
}

// trim_string_left removes a prefix string if present
// Not handled by codegen — real V implementation
pub fn (s string) trim_string_left(str string) string {
	if s.starts_with(str) {
		return s.substr(str.len, s.len)
	}
	return s
}

// trim_string_right removes a suffix string if present
// Not handled by codegen — real V implementation
pub fn (s string) trim_string_right(str string) string {
	if s.ends_with(str) {
		return s.substr(0, s.len - str.len)
	}
	return s
}

// === C-pointer string functions ===
// On BEAM, there are no raw C pointers. Instead, &u8/&char map to Erlang binaries.
// The codegen intercepts these calls and emits BEAM-native equivalents:
//   tos(s, len)              → binary:part(S, 0, Len)    — extract first len bytes
//   tos2/3/4/5(s)            → hd(binary:split(S, <<0>>)) — null-scan, return prefix
//   cstring_to_vstring(s)    → hd(binary:split(S, <<0>>)) — null-scan, return prefix
//   tos_clone(s)             → binary:copy(S)             — force independent copy
//
// This works because on BEAM, all byte data arrives as Erlang binaries (via Ports,
// NIFs, file I/O, network). The "pointer" IS the data — no indirection needed.
// For the native backend / OS layer, actual C memory is handled by the FFI layer
// and data crosses into BEAM world as binaries.

// cstring_to_vstring creates a V string from a null-terminated byte sequence.
// Codegen intercepts → hd(binary:split(S, <<0>>))
@[unsafe]
pub fn cstring_to_vstring(const_s &char) string {
	return ''
}

// tos_clone creates an independent copy of the byte data as a V string.
// Codegen intercepts → binary:copy(S)
@[unsafe]
pub fn tos_clone(const_s &u8) string {
	return ''
}

// tos creates a V string from the first `len` bytes of a byte sequence.
// Codegen intercepts → binary:part(S, 0, Len)
@[unsafe]
pub fn tos(s &u8, len int) string {
	return ''
}

// tos2 creates a V string from a null-terminated byte sequence.
// Codegen intercepts → hd(binary:split(S, <<0>>))
@[unsafe]
pub fn tos2(s &u8) string {
	return ''
}

// tos3 creates a V string from a null-terminated char sequence.
// Codegen intercepts → hd(binary:split(S, <<0>>))
@[unsafe]
pub fn tos3(s &char) string {
	return ''
}

// tos4 creates a V string from a null-terminated byte sequence.
// Codegen intercepts → hd(binary:split(S, <<0>>))
@[unsafe]
pub fn tos4(s &u8) string {
	return ''
}

// tos5 creates a V string from a null-terminated char sequence.
// Codegen intercepts → hd(binary:split(S, <<0>>))
@[unsafe]
pub fn tos5(s &char) string {
	return ''
}

// contains_any returns true if the string contains any of the chars
// Not handled by codegen — real V implementation using byte_in_str
pub fn (s string) contains_any(chars string) bool {
	if s.len == 0 || chars.len == 0 {
		return false
	}
	s_bytes := s.bytes()
	for b in s_bytes {
		if byte_in_str(b, chars) {
			return true
		}
	}
	return false
}

// bool parses the string as a boolean
pub fn (s string) bool() bool {
	return s == 'true' || s == '1'
}

// match_glob matches the string against a glob pattern
// Supports: * (match any), ? (match single char), literal chars
// Not handled by codegen — real V implementation
pub fn (s string) match_glob(pattern string) bool {
	if pattern == '*' {
		return true
	}
	if pattern == s {
		return true
	}
	// Handle patterns like "*.ext", "prefix*", "pre*suf"
	if pattern.contains('*') {
		// Split on first * — match prefix and suffix
		star_idx := pattern.index_('*')
		if star_idx == -1 {
			return pattern == s
		}
		prefix := pattern.substr(0, star_idx)
		suffix := pattern.substr(star_idx + 1, pattern.len)
		// Check prefix matches start
		if prefix.len > 0 && !s.starts_with(prefix) {
			return false
		}
		// Check suffix matches end (may contain more wildcards)
		if suffix.len > 0 {
			if suffix.contains('*') {
				// Recursive: match remaining pattern against remainder of string
				remainder := s.substr(prefix.len, s.len)
				return remainder.match_glob(suffix)
			}
			return s.ends_with(suffix) && s.len >= prefix.len + suffix.len
		}
		return true
	}
	// Handle ? wildcard — must match exactly one character
	if pattern.contains('?') {
		if s.len != pattern.len {
			return false
		}
		for i := 0; i < pattern.len; i++ {
			p_bytes := pattern.bytes()
			s_bytes := s.bytes()
			if p_bytes[i] != 63 && p_bytes[i] != s_bytes[i] { // 63 = '?'
				return false
			}
		}
		return true
	}
	return false
}

// split_once splits the string at the first occurrence of delim
pub fn (s string) split_once(delim string) ?(string, string) {
	idx := s.index_(delim)
	if idx == -1 {
		return none
	}
	return s.substr(0, idx), s.substr(idx + delim.len, s.len)
}

// split_nth splits the string by delim into at most n parts
// Not handled by codegen — real V implementation
pub fn (s string) split_nth(delim string, n int) []string {
	if n < 1 || s.len == 0 {
		return [s]
	}
	if n == 1 {
		return [s]
	}
	all := s.split(delim)
	if all.len <= n {
		return all
	}
	mut result := []string{}
	for i := 0; i < n - 1; i++ {
		result << all[i]
	}
	mut rest := all[n - 1]
	for i := n; i < all.len; i++ {
		rest = rest + delim + all[i]
	}
	result << rest
	return result
}

// split_any splits the string by any of the delimiter characters.
// Stub — complex split logic needs codegen support for efficient BEAM implementation.
// Falls back to split on first delim char for basic functionality.
@[direct_array_access]
pub fn (s string) split_any(delim string) []string {
	if s.len == 0 {
		return ['']
	}
	if delim.len == 0 {
		return [s]
	}
	// Use replace to normalize all delim chars to the first one, then split on that
	if delim.len == 1 {
		return s.split(delim)
	}
	// Replace all delimiter characters with the first one
	first_delim := delim.substr(0, 1)
	mut normalized := s
	for i := 1; i < delim.len; i++ {
		normalized = normalized.replace(delim.substr(i, i + 1), first_delim)
	}
	return normalized.split(first_delim)
}

// rsplit_any splits the string by any delimiter chars in reverse order.
// Not handled by codegen — real V implementation
@[direct_array_access]
pub fn (s string) rsplit_any(delim string) []string {
	result := s.split_any(delim)
	return result.reverse()
}

// find_between returns the string found between the two input strings.
// Not handled by codegen — real V implementation
pub fn (s string) find_between(start string, end string) string {
	start_idx := s.index_(start)
	if start_idx == -1 {
		return ''
	}
	after_start := start_idx + start.len
	remainder := s.substr(after_start, s.len)
	end_idx := remainder.index_(end)
	if end_idx == -1 {
		return ''
	}
	return remainder.substr(0, end_idx)
}

// find_between_pair_u8 returns the string found between the two input delimiter characters.
// Not handled by codegen — real V implementation
pub fn (s string) find_between_pair_u8(pair_start u8, pair_end u8) string {
	start_idx := s.index_u8(pair_start)
	if start_idx == -1 {
		return ''
	}
	after_start := start_idx + 1
	remainder := s.substr(after_start, s.len)
	end_idx := remainder.index_u8(pair_end)
	if end_idx == -1 {
		return ''
	}
	return remainder.substr(0, end_idx)
}

// find_between_pair_rune returns the string found between the two input delimiter runes.
// Converts runes to single-character strings and delegates to find_between.
// On BEAM, runes are integers (Unicode codepoints). We use string interpolation
// to convert them to their UTF-8 binary representation.
pub fn (s string) find_between_pair_rune(pair_start rune, pair_end rune) string {
	start_str := '${pair_start}'
	end_str := '${pair_end}'
	return s.find_between(start_str, end_str)
}

// find_between_pair_string returns the string found between the two input delimiter strings.
// Not handled by codegen — delegates to find_between
pub fn (s string) find_between_pair_string(pair_start string, pair_end string) string {
	return s.find_between(pair_start, pair_end)
}
