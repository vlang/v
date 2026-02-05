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
pub fn (n i8) str() string {
	return int(n).str()
}

// str returns the value of the `i16` as a `string`.
pub fn (n i16) str() string {
	return int(n).str()
}

// str returns the value of the `u16` as a `string`.
pub fn (n u16) str() string {
	return int(n).str()
}

// str returns the value of the `int` as a `string`.
pub fn (n int) str() string {
	// Beam backend: will use Erlang's integer_to_binary/1
	return ''
}

// str returns the value of the `i32` as a `string`.
pub fn (n i32) str() string {
	return int(n).str()
}

// str returns the value of the `u32` as a `string`.
pub fn (n u32) str() string {
	return u64(n).str()
}

// str returns the value of the `i64` as a `string`.
pub fn (n i64) str() string {
	// Beam backend: will use Erlang's integer_to_binary/1
	return ''
}

// str returns the value of the `u64` as a `string`.
pub fn (n u64) str() string {
	// Beam backend: will use Erlang's integer_to_binary/1
	return ''
}

// str returns the value of the `bool` as a `string`.
pub fn (b bool) str() string {
	if b {
		return 'true'
	}
	return 'false'
}

// str returns the value of the `u8` as a `string`.
pub fn (b u8) str() string {
	return int(b).str()
}

// str returns the value of the `int_literal` as a `string`.
pub fn (n int_literal) str() string {
	return i64(n).str()
}

// hex returns the value of the `u64` as a hexadecimal `string`.
pub fn (nn u64) hex() string {
	// Beam backend: will use Erlang's integer_to_list/2
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
pub fn (n int) hex2() string {
	return '0x' + n.hex()
}

// hex_full returns the full 16-digit hexadecimal representation
pub fn (nn u64) hex_full() string {
	// Beam backend: will use Erlang's io_lib:format
	return ''
}

// is_capital returns `true` if the byte is a Latin capital letter.
// Example: assert u8(`H`).is_capital() == true
// Example: assert u8(`h`).is_capital() == false
@[inline]
pub fn (c u8) is_capital() bool {
	return c >= `A` && c <= `Z`
}

// is_letter returns `true` if the byte is an ASCII letter.
@[inline]
pub fn (c u8) is_letter() bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
}

// is_digit returns `true` if the byte is an ASCII digit.
@[inline]
pub fn (c u8) is_digit() bool {
	return c >= `0` && c <= `9`
}

// is_alnum returns `true` if the byte is alphanumeric.
@[inline]
pub fn (c u8) is_alnum() bool {
	return c.is_letter() || c.is_digit()
}

// is_space returns `true` if the byte is whitespace.
@[inline]
pub fn (c u8) is_space() bool {
	return c in [` `, `\t`, `\n`, `\r`, `\v`, `\f`]
}

// ascii_str returns a single-character string
pub fn (b u8) ascii_str() string {
	// Beam backend stub
	return ''
}

// repeat returns a new string with count copies of the byte
pub fn (b u8) repeat(count int) string {
	// Beam backend stub
	return ''
}

// int_min returns the smallest `int` of input `a` and `b`.
// Example: assert int_min(2,3) == 2
@[inline]
pub fn int_min(a int, b int) int {
	return if a < b { a } else { b }
}

// int_max returns the largest `int` of input `a` and `b`.
// Example: assert int_max(2,3) == 3
@[inline]
pub fn int_max(a int, b int) int {
	return if a > b { a } else { b }
}

// ptr_str returns a string with the address of `ptr`.
// On BEAM: Pointers don't exist in the same way, returns a representation
pub fn ptr_str(ptr voidptr) string {
	// On BEAM, we can use Erlang's term formatting
	// This returns an opaque representation since BEAM doesn't have raw pointers
	return '<ptr>'
}

// vstring_with_len creates a string from a byte pointer with a given length
// On BEAM: Stub - binary creation would be handled by codegen
pub fn (b &u8) vstring_with_len(len int) string {
	// BEAM codegen handles this - would create binary from pointer + length
	return ''
}

// is_hex_digit returns true if the byte is a valid hexadecimal digit (0-9, a-f, A-F)
pub fn (c u8) is_hex_digit() bool {
	return (c >= `0` && c <= `9`) || (c >= `a` && c <= `f`) || (c >= `A` && c <= `F`)
}

// str_escaped returns a string representation of the byte with escape sequences for special chars
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
