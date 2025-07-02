module builtin

pub type byte = u8

pub const min_i8 = i8(-128)
pub const max_i8 = i8(127)

pub const min_i16 = i16(-32768)
pub const max_i16 = i16(32767)

pub const min_i32 = i32(-2147483648)
pub const max_i32 = i32(2147483647)

pub const min_int = min_i32
pub const max_int = max_i32

// -9223372036854775808 is wrong, because C compilers parse literal values
// without sign first, and 9223372036854775808 overflows i64, hence the
// consecutive subtraction by 1
pub const min_i64 = i64(-9223372036854775807 - 1)
pub const max_i64 = i64(9223372036854775807)

pub const min_u8 = u8(0)
pub const max_u8 = u8(255)

pub const min_u16 = u16(0)
pub const max_u16 = u16(65535)

pub const min_u32 = u32(0)
pub const max_u32 = u32(4294967295)

pub const min_u64 = u64(0)
pub const max_u64 = u64(18446744073709551615)

pub fn (i i8) str() string {
	mut res := ''
	#res.str = i.val.toString()

	return res
}

pub fn (i i16) str() string {
	mut res := ''
	#res.str = i.val.toString()

	return res
}

pub fn (i u16) str() string {
	mut res := ''
	#res.str = i.val.toString()

	return res
}

pub fn (i int) str() string {
	mut res := ''
	#res = new string( i+'' )

	return res
}

pub fn (i i64) str() string {
	mut res := ''
	#res = new string( i + '')

	return res
}

pub fn (i u32) str() string {
	mut res := ''
	#res = new string( i + '')

	return res
}

pub fn (i u64) str() string {
	mut res := ''
	#res = new string( i + '')

	return res
}

pub fn (i bool) str() string {
	mut res := ''
	#res = new string( i + '')

	return res
}

pub fn (i any) str() string {
	mut res := ''
	#res = new string( i.toString() )

	return res
}

pub fn (i int_literal) str() string {
	res := ''
	#res.str = i.val.toString()

	return res
}

pub fn (x u64) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x u64) hex_full() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x i64) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x u32) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x u16) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x i8) hex() string {
	mut res := ''
	#res.str = x.val.toString(16)
	if res.len < 2 {
		res = '0' + res
	}
	return res
}

pub fn (x i16) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x int) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x int_literal) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x u8) hex() string {
	mut res := ''
	#res.str = x.val.toString(16)
	if res.len < 2 {
		res = '0' + res
	}
	return res
}

// hex returns a string with the hexadecimal representation of the byte elements of the array.
pub fn (b []u8) hex() string {
	mut hex := ''
	for i in b {
		mut z := i
		z = z
		#let n0 = i.val >> 4
		#hex.str += n0 < 10 ? String.fromCharCode(n0) : String.fromCharCode(n0 + 87)

		#let n1 = i.val & 0xF
		#hex.str += n1 < 10 ? String.fromCharCode(n1) : String.fromCharCode(n1 + 87)
	}
	return hex
}

pub fn (i int) hex2() string {
	return '0x' + i.hex()
}

pub fn (i i8) hex2() string {
	return '0x' + i.hex()
}

pub fn (i i16) hex2() string {
	return '0x' + i.hex()
}

pub fn (i i64) hex2() string {
	return '0x' + i.hex()
}

pub fn (i u8) hex2() string {
	return '0x' + i.hex()
}

pub fn (i u16) hex2() string {
	return '0x' + i.hex()
}

pub fn (i u32) hex2() string {
	return '0x' + i.hex()
}

pub fn (i u64) hex2() string {
	return '0x' + i.hex()
}
