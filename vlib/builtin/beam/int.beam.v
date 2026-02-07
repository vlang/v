module builtin

pub type byte = u8

pub const min_i8 = i8(-128)
pub const max_i8 = i8(127)

pub const min_i16 = i16(-32768)
pub const max_i16 = i16(32767)

pub const min_i32 = i32(-2147483648)
pub const max_i32 = i32(2147483647)

// -9223372036854775808 is wrong, because C compilers parse literal values
// without sign first, and 9223372036854775808 overflows i64, hence the
// consecutive subtraction by 1
pub const min_i64 = i64(-9223372036854775807 - 1)
pub const max_i64 = i64(9223372036854775807)

pub const min_int = int(min_i32)
pub const max_int = int(max_i32)

pub const min_u8 = u8(0)
pub const max_u8 = u8(255)

pub const min_u16 = u16(0)
pub const max_u16 = u16(65535)

pub const min_u32 = u32(0)
pub const max_u32 = u32(4294967295)

pub const min_u64 = u64(0)
pub const max_u64 = u64(18446744073709551615)

// str returns the value of the `i8` as a `string`.
// Codegen handles: integer_to_binary(int(N))
pub fn (n i8) str() string {
	return int(n).str()
}

// str returns the value of the `i16` as a `string`.
// Codegen handles: integer_to_binary(int(N))
pub fn (n i16) str() string {
	return int(n).str()
}

// str returns the value of the `u16` as a `string`.
// Codegen handles: integer_to_binary(int(N))
pub fn (n u16) str() string {
	return int(n).str()
}

// str returns the value of the `int` as a `string`.
// Codegen handles: integer_to_binary(N)
pub fn (n int) str() string {
	return ''
}

// str returns the value of the `i32` as a `string`.
// Codegen handles: integer_to_binary(int(N))
pub fn (n i32) str() string {
	return int(n).str()
}

// str returns the value of the `u32` as a `string`.
// Codegen handles: integer_to_binary(N)
pub fn (n u32) str() string {
	return u64(n).str()
}

// str returns the value of the `i64` as a `string`.
// Codegen handles: integer_to_binary(N)
pub fn (n i64) str() string {
	return ''
}

// str returns the value of the `u64` as a `string`.
// Codegen handles: integer_to_binary(N)
pub fn (n u64) str() string {
	return ''
}

// str returns the value of the `bool` as a `string`.
// Codegen handles: atom_to_binary(B)
pub fn (b bool) str() string {
	if b {
		return 'true'
	}
	return 'false'
}

// str returns the value of the `u8` as a `string`.
// Codegen handles: integer_to_binary(int(N))
pub fn (b u8) str() string {
	return int(b).str()
}

// str returns the value of the `int_literal` as a `string`.
pub fn (n int_literal) str() string {
	return i64(n).str()
}

// hex returns the value of the `u64` as a hexadecimal `string`.
// Codegen handles: integer_to_binary(N, 16)
pub fn (nn u64) hex() string {
	return ''
}

// hex returns the value of the `i64` as a hexadecimal `string`.
pub fn (nn i64) hex() string {
	return u64(nn).hex()
}

// hex returns the value of the `u32` as a hexadecimal `string`.
pub fn (nn u32) hex() string {
	return u64(nn).hex()
}

// hex returns the value of the `int` as a hexadecimal `string`.
pub fn (nn int) hex() string {
	return u32(nn).hex()
}

// hex returns the value of the `u16` as a hexadecimal `string`.
pub fn (nn u16) hex() string {
	return u64(nn).hex()
}

// hex returns the value of the `i16` as a hexadecimal `string`.
pub fn (nn i16) hex() string {
	return u16(nn).hex()
}

// hex returns the value of the `u8` as a hexadecimal `string`.
pub fn (nn u8) hex() string {
	return u64(nn).hex()
}

// hex returns the value of the `i8` as a hexadecimal `string`.
pub fn (nn i8) hex() string {
	return u64(nn).hex()
}

// hex returns the value of the `int_literal` as a hexadecimal `string`.
pub fn (nn int_literal) hex() string {
	return u64(nn).hex()
}

// hex2 returns a 0x-prefixed hexadecimal string
// Not handled by codegen — real V implementation
pub fn (n int) hex2() string {
	return '0x' + n.hex()
}

// hex_full returns the full 16-digit hexadecimal representation
// Not handled by codegen — real V implementation
pub fn (nn u64) hex_full() string {
	h := nn.hex()
	// Pad with leading zeros to 16 digits
	if h.len >= 16 {
		return h
	}
	pad := '0'.repeat(16 - h.len)
	return pad + h
}

// is_capital returns `true` if the byte is a Latin capital letter.
// Codegen handles: range check 65-90
@[inline]
pub fn (c u8) is_capital() bool {
	return c >= `A` && c <= `Z`
}

// is_letter returns `true` if the byte is an ASCII letter.
// Codegen handles: range check A-Z || a-z
@[inline]
pub fn (c u8) is_letter() bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
}

// is_digit returns `true` if the byte is an ASCII digit.
// Codegen handles: range check 48-57
@[inline]
pub fn (c u8) is_digit() bool {
	return c >= `0` && c <= `9`
}

// is_alnum returns `true` if the byte is alphanumeric.
// Not handled by codegen — real V implementation (calls is_letter/is_digit which codegen handles)
@[inline]
pub fn (c u8) is_alnum() bool {
	return c.is_letter() || c.is_digit()
}

// is_space returns `true` if the byte is whitespace.
// Codegen handles: value equality checks for 32, 9, 10, 13
@[inline]
pub fn (c u8) is_space() bool {
	return c == ` ` || c == `\t` || c == `\n` || c == `\r` || c == `\v` || c == `\f`
}

// ascii_str returns a single-character string
// Codegen handles: list_to_binary([X])
pub fn (b u8) ascii_str() string {
	return ''
}

// repeat returns a new string with count copies of the byte
// Not handled by codegen — real V implementation
pub fn (b u8) repeat(count int) string {
	if count <= 0 {
		return ''
	}
	ch := b.ascii_str()
	return ch.repeat(count)
}

// int_min returns the smallest `int` of input `a` and `b`.
@[inline]
pub fn int_min(a int, b int) int {
	return if a < b { a } else { b }
}

// int_max returns the largest `int` of input `a` and `b`.
@[inline]
pub fn int_max(a int, b int) int {
	return if a > b { a } else { b }
}

// ptr_str returns a string with the address of `ptr`.
// On BEAM: Pointers don't exist in the same way, returns a representation
pub fn ptr_str(ptr voidptr) string {
	return '<ptr>'
}

// vstring_with_len creates a string from a byte pointer with a given length
// Stub — BEAM doesn't have raw pointers.
pub fn (b &u8) vstring_with_len(len int) string {
	return ''
}

// vstring_literal_with_len creates a string from a byte pointer with a given length
// Stub — BEAM doesn't have raw pointers.
pub fn (b &u8) vstring_literal_with_len(len int) string {
	return ''
}

// vbytes creates a byte slice from a pointer with a given length
// Stub — BEAM doesn't have raw pointers.
pub fn (b &u8) vbytes(len int) []u8 {
	return []
}

// vbytes on `voidptr` makes a V []u8 structure from a memory buffer.
// Stub — BEAM doesn't have raw pointers.
@[unsafe]
pub fn (data voidptr) vbytes(len int) []u8 {
	return []
}

// is_hex_digit returns true if the byte is a valid hexadecimal digit (0-9, a-f, A-F)
// Not handled by codegen — real V implementation
pub fn (c u8) is_hex_digit() bool {
	return (c >= `0` && c <= `9`) || (c >= `a` && c <= `f`) || (c >= `A` && c <= `F`)
}

// str_escaped returns a string representation of the byte with escape sequences for special chars
// Not handled by codegen — real V implementation
pub fn (b u8) str_escaped() string {
	match b {
		`\n` { return '\\n' }
		`\r` { return '\\r' }
		`\t` { return '\\t' }
		`\\` { return '\\\\' }
		`"` { return '\\"' }
		`'` { return "\\'" }
		else {
			if b < 32 || b > 126 {
				return '\\x' + b.hex()
			}
			return b.ascii_str()
		}
	}
}
