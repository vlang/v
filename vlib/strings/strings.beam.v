// BEAM backend strings functions
// These functions are translated by the BEAM codegen to Erlang runtime calls
module strings

// repeat returns a string with the byte `c` repeated `n` times
// Codegen translates to: lists:duplicate(N, C) or binary:copy
pub fn repeat(c u8, n int) string {
	if n <= 0 {
		return ''
	}
	// BEAM codegen handles this - translates to list/binary operations
	mut arr := []u8{cap: n}
	for _ in 0 .. n {
		arr << c
	}
	return arr.bytestr()
}

// repeat_string returns the string `s` repeated `n` times
// Codegen translates to: binary:copy(S, N) or list concatenation
pub fn repeat_string(s string, n int) string {
	if n <= 0 || s.len == 0 {
		return ''
	}
	// BEAM codegen handles this - translates to efficient binary/list ops
	mut result := strings.new_builder(s.len * n)
	for _ in 0 .. n {
		result.write_string(s)
	}
	return result.str()
}
