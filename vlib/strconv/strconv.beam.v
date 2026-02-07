// BEAM backend strconv implementation
// Uses Erlang's built-in string-to-number conversion
//
// IMPORTANT: The BEAM codegen intercepts these calls directly:
//   atoi(s)   -> erlang:binary_to_integer(S)
//   atof64(s) -> erlang:binary_to_float(S)
//   int.str() -> erlang:integer_to_binary(N)
//   f64.str() -> erlang:float_to_binary(F)
//
// The functions below are fallbacks for when codegen does NOT intercept
// (e.g., format_int, format_uint, ftoa_* which are called through
// strconv module rather than as method calls).
module strconv

// AtoF64Param - parameters for atof64
@[params]
pub struct AtoF64Param {
pub:
	allow_extra_chars bool // allow extra characters after number
}

// atof64 parses a string to f64
// Codegen intercepts: erlang:binary_to_float(S)
pub fn atof64(s string, param AtoF64Param) !f64 {
	if s.len == 0 {
		return error('expected a number found an empty string')
	}
	// Fallback stub - codegen should intercept this call
	return 0.0
}

// atof_quick quickly parses a string to f64 (no error checking)
// Codegen should intercept: erlang:binary_to_float(S)
pub fn atof_quick(s string) f64 {
	return 0.0
}

// format_int formats an integer to string with given radix
// NOT intercepted by codegen - this is a real implementation
// On BEAM: would use erlang:integer_to_binary(N, Radix)
pub fn format_int(n i64, radix int) string {
	if n == 0 {
		return '0'
	}
	mut num := if n < 0 { -n } else { n }
	digits := '0123456789abcdefghijklmnopqrstuvwxyz'
	mut buf := []u8{}
	for num > 0 {
		remainder := int(num % i64(radix))
		buf << digits[remainder]
		num = num / i64(radix)
	}
	if n < 0 {
		buf << u8(`-`)
	}
	// Reverse the buffer
	mut result := []u8{len: buf.len}
	for i in 0 .. buf.len {
		result[i] = buf[buf.len - 1 - i]
	}
	return result.bytestr()
}

// format_uint formats an unsigned integer to string with given radix
// NOT intercepted by codegen - this is a real implementation
pub fn format_uint(n u64, radix int) string {
	if n == 0 {
		return '0'
	}
	mut num := n
	digits := '0123456789abcdefghijklmnopqrstuvwxyz'
	mut buf := []u8{}
	for num > 0 {
		remainder := int(num % u64(radix))
		buf << digits[remainder]
		num = num / u64(radix)
	}
	// Reverse the buffer
	mut result := []u8{len: buf.len}
	for i in 0 .. buf.len {
		result[i] = buf[buf.len - 1 - i]
	}
	return result.bytestr()
}

// ftoa_64 converts f64 to string
// NOT intercepted by codegen
// On BEAM: erlang:float_to_binary(F, [{decimals, 6}, compact])
pub fn ftoa_64(f f64) string {
	// Use the f64.str() method which codegen maps to float_to_binary
	return f.str()
}

// ftoa_long_64 converts f64 to string with full precision
pub fn ftoa_long_64(f f64) string {
	return f.str()
}

// ftoa_32 converts f32 to string
pub fn ftoa_32(f f32) string {
	return f64(f).str()
}

// ftoa_long_32 converts f32 to string with full precision
pub fn ftoa_long_32(f f32) string {
	return f64(f).str()
}

// f64_to_str converts f64 to string with specified digits
// n_digit is the number of significant digits
pub fn f64_to_str(f f64, n_digit int) string {
	return f.str()
}

// f64_to_str_pad converts f64 to string with padding
pub fn f64_to_str_pad(f f64, n_digit int) string {
	return f.str()
}

// f32_to_str converts f32 to string with specified digits
pub fn f32_to_str(f f32, n_digit int) string {
	return f64(f).str()
}

// f32_to_str_pad converts f32 to string with padding
pub fn f32_to_str_pad(f f32, n_digit int) string {
	return f64(f).str()
}

// f64_to_str_l converts f64 to string long format
pub fn f64_to_str_l(f f64) string {
	return f.str()
}

// f64_to_str_l_with_dot converts f64 to string with decimal point
pub fn f64_to_str_l_with_dot(f f64) string {
	s := f.str()
	// Ensure there is a decimal point
	mut has_dot := false
	for i in 0 .. s.len {
		if s[i] == u8(`.`) {
			has_dot = true
			break
		}
	}
	if !has_dot {
		return s + '.0'
	}
	return s
}

// f32_to_str_l converts f32 to string long format
pub fn f32_to_str_l(f f32) string {
	return f64(f).str()
}

// f32_to_str_l_with_dot converts f32 to string with decimal point
pub fn f32_to_str_l_with_dot(f f32) string {
	return f64_to_str_l_with_dot(f64(f))
}

// fxx_to_str_l_parse parses float string for formatting
pub fn fxx_to_str_l_parse(s string) string {
	return s
}

// fxx_to_str_l_parse_with_dot parses float string ensuring decimal point
pub fn fxx_to_str_l_parse_with_dot(s string) string {
	mut has_dot := false
	for i in 0 .. s.len {
		if s[i] == u8(`.`) {
			has_dot = true
			break
		}
	}
	if !has_dot {
		return s + '.0'
	}
	return s
}

// f64_to_str_lnd1 converts f64 to string with decimal digits
pub fn f64_to_str_lnd1(f f64, dec_digit int) string {
	return f.str()
}

// remove_tail_zeros removes trailing zeros from number string
// e.g., "3.14000" -> "3.14", "100.0" -> "100.0" (keeps at least one decimal)
pub fn remove_tail_zeros(s string) string {
	if s.len == 0 {
		return s
	}
	// Find the decimal point
	mut dot_pos := -1
	for i in 0 .. s.len {
		if s[i] == u8(`.`) {
			dot_pos = i
			break
		}
	}
	if dot_pos == -1 {
		return s // No decimal point, nothing to trim
	}
	// Find last non-zero position after decimal
	mut last_nonzero := s.len - 1
	for last_nonzero > dot_pos {
		if s[last_nonzero] != u8(`0`) {
			break
		}
		last_nonzero--
	}
	// Keep at least one digit after decimal point
	if last_nonzero == dot_pos {
		last_nonzero++
	}
	return s[..last_nonzero + 1]
}

// v_printf formatted print
// On BEAM: basic implementation using io:format
pub fn v_printf(str string, pt ...voidptr) {
	// Stub - codegen should handle println/print directly
	// Complex printf formatting is not yet supported on BEAM
}

// v_sprintf formatted string
// On BEAM: basic implementation
pub fn v_sprintf(str string, pt ...voidptr) string {
	// Stub - complex sprintf formatting is not yet supported on BEAM
	return str
}
