// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

pub struct VContext {
	allocator int
}

pub type byte = u8

// ptr_str returns a string with the address of `ptr`.
pub fn ptr_str(ptr voidptr) string {
	buf1 := u64_to_hex_no_leading_zeros(u64(ptr), 16)
	return buf1
}

// str returns the string equivalent of x.
pub fn (x isize) str() string {
	return i64(x).str()
}

// str returns the string equivalent of x.
pub fn (x usize) str() string {
	return u64(x).str()
}

// str returns a string with the address stored in the pointer cptr.
pub fn (cptr &char) str() string {
	return u64(cptr).hex()
}

// digit pairs in reverse order
const digit_pairs = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'

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

pub const min_int = $if new_int ?&& x64 { int(min_i64) } $else { int(min_i32) }
pub const max_int = $if new_int ?&& x64 { int(max_i64) } $else { int(max_i32) }

pub const min_u8 = u8(0)
pub const max_u8 = u8(255)

pub const min_u16 = u16(0)
pub const max_u16 = u16(65535)

pub const min_u32 = u32(0)
pub const max_u32 = u32(4294967295)

pub const min_u64 = u64(0)
pub const max_u64 = u64(18446744073709551615)

// str_l returns the string representation of the integer nn with max chars.
@[direct_array_access; inline]
fn (nn int) str_l(max int) string {
	// This implementation is the quickest with gcc -O2
	unsafe {
		mut n := i64(nn)
		mut d := 0
		if n == 0 {
			return '0'
		}

		// overflow protect
		$if new_int ?&& x64 {
			if n == min_i64 {
				return '-9223372036854775808'
			}
		} $else {
			if n == min_i32 {
				return '-2147483648'
			}
		}

		mut is_neg := false
		if n < 0 {
			n = -n
			is_neg = true
		}
		mut index := max
		mut buf := malloc_noscan(max + 1)
		buf[index] = 0
		index--

		for n > 0 {
			n1 := int(n / 100)
			// calculate the digit_pairs start index
			d = int(u32(int(n) - (n1 * 100)) << 1)
			n = n1
			buf[index] = digit_pairs.str[d]
			index--
			d++
			buf[index] = digit_pairs.str[d]
			index--
		}
		index++
		// remove head zero
		if d < 20 {
			index++
		}
		// Prepend - if it's negative
		if is_neg {
			index--
			buf[index] = `-`
		}
		diff := max - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)
	}
}

// str returns the value of the `i8` as a `string`.
// Example: assert i8(-2).str() == '-2'
pub fn (n i8) str() string {
	return int(n).str_l(4)
}

// str returns the value of the `i16` as a `string`.
// Example: assert i16(-20).str() == '-20'
pub fn (n i16) str() string {
	return int(n).str_l(6)
}

// str returns the value of the `u16` as a `string`.
// Example: assert u16(20).str() == '20'
pub fn (n u16) str() string {
	return int(n).str_l(6)
}

// str returns the value of the `i32` as a `string`.
// Example: assert i32(-32769).str() == '-32769'
pub fn (n i32) str() string {
	return int(n).str_l(11)
}

// hex_full returns the value of the `int` as a *full* 8-digit hexadecimal `string`.
// Example: assert int(20).hex_full() == '00000014'
// Example: assert int(-20).hex_full() == 'ffffffec'
pub fn (nn int) hex_full() string {
	return u64_to_hex(u64(nn), 8)
}

// str returns the value of the `int` as a `string`.
// Example: assert int(-2020).str() == '-2020'
pub fn (n int) str() string {
	$if new_int ? {
		return impl_i64_to_string(n)
	} $else {
		return n.str_l(11)
	}
}

// str returns the value of the `u32` as a `string`.
// Example: assert u32(20000).str() == '20000'
@[direct_array_access; inline]
pub fn (nn u32) str() string {
	unsafe {
		mut n := nn
		mut d := u32(0)
		if n == 0 {
			return '0'
		}
		max := 10
		mut buf := malloc_noscan(max + 1)
		mut index := max
		buf[index] = 0
		index--
		for n > 0 {
			n1 := n / u32(100)
			d = ((n - (n1 * u32(100))) << u32(1))
			n = n1
			buf[index] = digit_pairs[int(d)]
			index--
			d++
			buf[index] = digit_pairs[int(d)]
			index--
		}
		index++
		// remove head zero
		if d < u32(20) {
			index++
		}
		diff := max - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)
	}
}

// str returns the value of the `u128` as a `string`.
// Example: assert u128(20000).str() == '20000'
pub fn (nn u128) str() string {
	if nn == u128(0) {
		return '0'
	}
	mut n := nn
	mut buf := []u8{len: 40}
	mut index := buf.len
	for n != u128(0) {
		index--
		buf[index] = u8(48) + u8(n % u128(10))
		n = n / u128(10)
	}
	return unsafe { tos(&buf[index], buf.len - index) }
}

// str returns the value of the `i128` as a `string`.
// Example: assert i128(-20000).str() == '-20000'
pub fn (nn i128) str() string {
	if nn == i128(0) {
		return '0'
	}
	negative := nn < i128(0)
	// The magnitude comes from the unsigned negation: negating the minimum value
	// has no signed counterpart, but its bit pattern is the magnitude.
	magnitude := if negative { u128(-nn) } else { u128(nn) }
	text := magnitude.str()
	return if negative { '-' + text } else { text }
}

// str returns the value of the `int_literal` as a `string`.
@[inline]
pub fn (n int_literal) str() string {
	return impl_i64_to_string(n)
}

// str returns the value of the `i64` as a `string`.
// Example: assert i64(-200000).str() == '-200000'
@[inline]
pub fn (nn i64) str() string {
	return impl_i64_to_string(nn)
}

@[direct_array_access]
fn impl_i64_to_string(nn i64) string {
	unsafe {
		mut n := nn
		mut d := i64(0)
		if n == 0 {
			return '0'
		} else if n == min_i64 {
			return '-9223372036854775808'
		}
		max := 20
		mut buf := malloc_noscan(max + 1)
		mut is_neg := false
		if n < 0 {
			n = -n
			is_neg = true
		}
		mut index := max
		buf[index] = 0
		index--
		for n > 0 {
			n1 := n / i64(100)
			d = (u32(n - (n1 * i64(100))) << i64(1))
			n = n1
			buf[index] = digit_pairs[int(d)]
			index--
			d++
			buf[index] = digit_pairs[int(d)]
			index--
		}
		index++
		// remove head zero
		if d < i64(20) {
			index++
		}
		// Prepend - if it's negative
		if is_neg {
			index--
			buf[index] = `-`
		}
		diff := max - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)
	}
}

// str returns the value of the `u64` as a `string`.
// Example: assert u64(2000000).str() == '2000000'
@[direct_array_access; inline]
pub fn (nn u64) str() string {
	unsafe {
		mut n := nn
		mut d := u64(0)
		if n == 0 {
			return '0'
		}
		max := 20
		mut buf := malloc_noscan(max + 1)
		mut index := max
		buf[index] = 0
		index--
		for n > 0 {
			n1 := n / 100
			d = ((n - (n1 * 100)) << 1)
			n = n1
			buf[index] = digit_pairs[int(d)]
			index--
			d++
			buf[index] = digit_pairs[int(d)]
			index--
		}
		index++
		// remove head zero
		if d < 20 {
			index++
		}
		diff := max - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)
	}
}

// str returns the value of the `bool` as a `string`.
// Example: assert (2 > 1).str() == 'true'
pub fn (b bool) str() string {
	if b {
		return 'true'
	}
	return 'false'
}

// u64_to_hex converts the number `nn` to a (zero padded if necessary) hexadecimal `string`.
@[direct_array_access; inline]
fn u64_to_hex(nn u64, len u8) string {
	mut n := nn
	mut buf := [17]u8{}
	buf[len] = 0
	mut i := 0
	for i = len - 1; i >= 0; i-- {
		d := u8(n & 0xF)
		buf[i] = if d < 10 { d + `0` } else { d + 87 }
		n = n >> 4
	}
	return unsafe { tos(memdup(&buf[0], len + 1), len) }
}

// u64_to_hex_no_leading_zeros converts the number `nn` to hexadecimal `string`.
@[direct_array_access; inline]
fn u64_to_hex_no_leading_zeros(nn u64, len u8) string {
	mut n := nn
	mut buf := [17]u8{}
	buf[len] = 0
	mut i := 0
	for i = len - 1; i >= 0; i-- {
		d := u8(n & 0xF)
		buf[i] = if d < 10 { d + `0` } else { d + 87 }
		n = n >> 4
		if n == 0 {
			break
		}
	}
	res_len := len - i
	return unsafe { tos(memdup(&buf[i], res_len + 1), res_len) }
}

// hex returns the value of the `byte` as a hexadecimal `string`.
// Note that the output is zero padded for values below 16.
// Example: assert u8(2).hex() == '02'
// Example: assert u8(15).hex() == '0f'
// Example: assert u8(255).hex() == 'ff'
pub fn (nn u8) hex() string {
	if nn == 0 {
		return '00'
	}
	return u64_to_hex(nn, 2)
}

// hex returns a hexadecimal representation of `c` (as an 8 bit unsigned number).
// The output is zero padded for values below 16.
// Example: assert char(`A`).hex() == '41'
// Example: assert char(`Z`).hex() == '5a'
// Example: assert char(` `).hex() == '20'
pub fn (c char) hex() string {
	return u8(c).hex()
}

// hex returns a hexadecimal representation of the rune `r` (as a 32 bit unsigned number).
// Example: assert `A`.hex() == '41'
// Example: assert `💣`.hex() == '1f4a3'
pub fn (r rune) hex() string {
	return u32(r).hex()
}

// hex returns the value of the `i8` as a hexadecimal `string`.
// Note that the output is zero padded for values below 16.
// Example: assert i8(8).hex() == '08'
// Example: assert i8(10).hex() == '0a'
// Example: assert i8(15).hex() == '0f'
pub fn (nn i8) hex() string {
	if nn == 0 {
		return '00'
	}
	return u64_to_hex(u64(nn), 2)
}

// hex returns the value of the `u16` as a hexadecimal `string`.
// Note that the output is ***not*** zero padded.
// Example: assert u16(2).hex() == '2'
// Example: assert u16(200).hex() == 'c8'
pub fn (nn u16) hex() string {
	if nn == 0 {
		return '0'
	}
	return u64_to_hex_no_leading_zeros(nn, 4)
}

// hex returns the value of the `i16` as a hexadecimal `string`.
// Note that the output is ***not*** zero padded.
// Example: assert i16(2).hex() == '2'
// Example: assert i16(200).hex() == 'c8'
pub fn (nn i16) hex() string {
	return u16(nn).hex()
}

// hex returns the value of the `u32` as a hexadecimal `string`.
// Note that the output is ***not*** zero padded.
// Example: assert u32(2).hex() == '2'
// Example: assert u32(200).hex() == 'c8'
pub fn (nn u32) hex() string {
	if nn == 0 {
		return '0'
	}
	return u64_to_hex_no_leading_zeros(nn, 8)
}

// hex returns the value of the `int` as a hexadecimal `string`.
// Note that the output is ***not*** zero padded.
// Example: assert int(2).hex() == '2'
// Example: assert int(200).hex() == 'c8'
pub fn (nn int) hex() string {
	return u32(nn).hex()
}

// hex2 returns the value of the `int` as a `0x`-prefixed hexadecimal `string`.
// Note that the output after `0x` is ***not*** zero padded.
// Example: assert int(8).hex2() == '0x8'
// Example: assert int(15).hex2() == '0xf'
// Example: assert int(18).hex2() == '0x12'
pub fn (n int) hex2() string {
	return '0x' + n.hex()
}

// hex returns the value of the `u64` as a hexadecimal `string`.
// Note that the output is ***not*** zero padded.
// Example: assert u64(2).hex() == '2'
// Example: assert u64(2000).hex() == '7d0'
pub fn (nn u64) hex() string {
	if nn == 0 {
		return '0'
	}
	return u64_to_hex_no_leading_zeros(nn, 16)
}

// hex returns the value of the `i64` as a hexadecimal `string`.
// Note that the output is ***not*** zero padded.
// Example: assert i64(2).hex() == '2'
// Example: assert i64(-200).hex() == 'ffffffffffffff38'
// Example: assert i64(2021).hex() == '7e5'
pub fn (nn i64) hex() string {
	return u64(nn).hex()
}

// hex returns the value of the `int_literal` as a hexadecimal `string`.
// Note that the output is ***not*** zero padded.
// Example: assert 255.hex() == 'ff'
pub fn (nn int_literal) hex() string {
	return u64(nn).hex()
}

// str returns the value of the `voidptr` as a '0x'-prefixed hexadecimal `string`.
// Note that the output after '0x' is ***not*** zero padded.
pub fn (nn voidptr) str() string {
	return '0x' + u64(nn).hex()
}

// str returns the value of the `byteptr` as a '0x'-prefixed hexadecimal `string`.
// Note that the output after '0x' is ***not*** zero padded.
pub fn (nn byteptr) str() string {
	return '0x' + u64(nn).hex()
}

// str returns the value of the `charptr` as a '0x'-prefixed hexadecimal `string`.
// Note that the output after '0x' is ***not*** zero padded.
pub fn (nn charptr) str() string {
	return '0x' + u64(nn).hex()
}

// hex_full returns the value of the `u8` as a *full* 2-digit hexadecimal `string`.
// Example: assert u8(5).hex_full() == '05'
pub fn (nn u8) hex_full() string {
	return u64_to_hex(u64(nn), 2)
}

// hex_full returns the value of the `i8` as a *full* 2-digit hexadecimal `string`.
// Example: assert i8(-5).hex_full() == 'fb'
pub fn (nn i8) hex_full() string {
	return u64_to_hex(u64(nn), 2)
}

// hex_full returns the value of the `u16` as a *full* 4-digit hexadecimal `string`.
// Example: assert u16(5).hex_full() == '0005'
pub fn (nn u16) hex_full() string {
	return u64_to_hex(u64(nn), 4)
}

// hex_full returns the value of the `i16` as a *full* 4-digit hexadecimal `string`.
// Example: assert i16(-5).hex_full() == 'fffb'
pub fn (nn i16) hex_full() string {
	return u64_to_hex(u64(nn), 4)
}

// hex_full returns the value of the `u32` as a *full* 8-digit hexadecimal `string`.
// Example: assert u32(5).hex_full() == '00000005'
pub fn (nn u32) hex_full() string {
	return u64_to_hex(u64(nn), 8)
}

// hex_full returns the value of the `i64` as a *full* 16-digit hexadecimal `string`.
// Example: assert i64(-5).hex_full() == 'fffffffffffffffb'
pub fn (nn i64) hex_full() string {
	return u64_to_hex(u64(nn), 16)
}

// hex_full returns the value of the `voidptr` as a *full* 16-digit hexadecimal `string`.
// Note that the output has ***no*** '0x' prefix, unlike `voidptr.str()`.
// Example: assert voidptr(255).hex_full() == '00000000000000ff'
pub fn (nn voidptr) hex_full() string {
	return u64_to_hex(u64(nn), 16)
}

// hex_full returns the value of the `int_literal` as a *full* 16-digit hexadecimal `string`.
// Example: assert 255.hex_full() == '00000000000000ff'
pub fn (nn int_literal) hex_full() string {
	return u64_to_hex(u64(nn), 16)
}

// hex_full returns the value of the `u64` as a *full* 16-digit hexadecimal `string`.
// Example: assert u64(2).hex_full() == '0000000000000002'
// Example: assert u64(255).hex_full() == '00000000000000ff'
pub fn (nn u64) hex_full() string {
	return u64_to_hex(nn, 16)
}

// hex returns the value of the `u128` as a hexadecimal `string`.
// Note that the output is ***not*** zero padded.
// Example: assert u128(255).hex() == 'ff'
pub fn (nn u128) hex() string {
	if nn == u128(0) {
		return '0'
	}
	return u128_to_hex(nn, 32, false)
}

// hex_full returns the value of the `u128` as a full 32-digit hexadecimal `string`.
// Example: assert u128(255).hex_full() == '000000000000000000000000000000ff'
pub fn (nn u128) hex_full() string {
	return u128_to_hex(nn, 32, true)
}

// hex returns the value of the `i128` as a hexadecimal `string`, the two's
// complement bits of the value.
// Example: assert i128(-1).hex() == 'ffffffffffffffffffffffffffffffff'
pub fn (nn i128) hex() string {
	return u128(nn).hex()
}

// bin returns the value of the `u128` as a binary `string`.
// Note that the output is ***not*** zero padded.
// Example: assert u128(5).bin() == '101'
pub fn (nn u128) bin() string {
	if nn == u128(0) {
		return '0'
	}
	return u128_to_bin(nn)
}

// str_base writes the value in the given base, using `a` to `f` for the digits
// above nine. Interpolation reaches for this on `x`, `o` and `b`, where the
// 64-bit path has nothing to call but a cast down to the low half.
pub fn (nn u128) str_base(base int) string {
	if nn == u128(0) {
		return '0'
	}
	divisor := u128(base)
	mut digits := []u8{}
	mut value := nn
	for value > u128(0) {
		digit := u8(value % divisor)
		digits << if digit < 10 { `0` + digit } else { `a` + digit - 10 }
		value = value / divisor
	}
	mut out := []u8{len: digits.len}
	for i, digit in digits {
		out[digits.len - 1 - i] = digit
	}
	return out.bytestr()
}

// char_str writes the code point in the low bits as text. Interpolation reaches
// for this on `c`; the cast the 64-bit path would use is one the transform cannot
// put on a 128-bit value, since the C representation of one is a struct.
pub fn (nn u128) char_str() string {
	return rune(nn).str()
}

// char_str writes the code point in the low bits as text, from the bit pattern, so
// a negative value prints the same code point as its unsigned counterpart.
pub fn (nn i128) char_str() string {
	return rune(nn).str()
}

// u128_to_hex writes the value into `max_digits` hexadecimal digits, most
// significant first, and drops the leading zeros unless `full` asks for them.
// Four bits at a time keeps this working on the portable representation, which
// carries no 128-bit arithmetic of its own.
fn u128_to_hex(nn u128, max_digits int, full bool) string {
	mut buf := []u8{len: max_digits}
	mut value := nn
	for i := max_digits - 1; i >= 0; i-- {
		digit := u8(value & u128(0xF))
		buf[i] = if digit < 10 { `0` + digit } else { `a` + digit - u8(10) }
		value = value >> 4
	}
	mut start := 0
	if !full {
		for start < max_digits - 1 && buf[start] == `0` {
			start++
		}
	}
	return buf[start..].bytestr()
}

fn u128_to_bin(nn u128) string {
	mut buf := []u8{len: 128}
	mut value := nn
	for i := 127; i >= 0; i-- {
		buf[i] = if value & u128(1) == u128(1) { `1` } else { `0` }
		value = value >> 1
	}
	mut start := 0
	for start < 127 && buf[start] == `0` {
		start++
	}
	return buf[start..].bytestr()
}

// str returns the contents of `byte` as a zero terminated `string`.
// See also: [`byte.ascii_str`](#byte.ascii_str)
// Example: assert u8(111).str() == '111'
pub fn (b u8) str() string {
	return int(b).str_l(4)
}

// ascii_str returns the contents of `byte` as a zero terminated ASCII `string` character.
// Example: assert u8(97).ascii_str() == 'a'
pub fn (b u8) ascii_str() string {
	mut str := string{
		str: unsafe { malloc_noscan(2) }
		len: 1
	}
	unsafe {
		str.str[0] = b
		str.str[1] = 0
	}
	return str
}

// str_escaped returns the contents of `byte` as an escaped `string`.
// Example: assert u8(0).str_escaped() == r'`\0`'
@[manualfree]
pub fn (b u8) str_escaped() string {
	str := match b {
		0 {
			r'`\0`'
		}
		7 {
			r'`\a`'
		}
		8 {
			r'`\b`'
		}
		9 {
			r'`\t`'
		}
		10 {
			r'`\n`'
		}
		11 {
			r'`\v`'
		}
		12 {
			r'`\f`'
		}
		13 {
			r'`\r`'
		}
		27 {
			r'`\e`'
		}
		32...126 {
			b.ascii_str()
		}
		else {
			xx := b.hex()
			yy := '0x' + xx
			unsafe { xx.free() }
			yy
		}
	}

	return str
}

// is_capital returns `true`, if the byte is a Latin capital letter.
// Example: assert u8(`H`).is_capital() == true
// Example: assert u8(`h`).is_capital() == false
@[inline]
pub fn (c u8) is_capital() bool {
	return c >= `A` && c <= `Z`
}

// bytestr produces a string from *all* the bytes in the array.
// Note: the returned string will have .len equal to the array.len,
// even when some of the array bytes were `0`.
// If you want to get a V string, that contains only the bytes till
// the first `0` byte, use `tos_clone(&u8(array.data))` instead.
pub fn (b []u8) bytestr() string {
	unsafe {
		buf := malloc_noscan(b.len + 1)
		vmemcpy(buf, b.data, b.len)
		buf[b.len] = 0
		return tos(buf, b.len)
	}
}

// byterune attempts to decode a sequence of bytes, from utf8 to utf32.
// It return the result as a rune.
// It will produce an error, if there are more than four bytes in the array.
pub fn (b []u8) byterune() !rune {
	r := b.utf8_to_utf32()!
	return rune(r)
}

// repeat returns a new string with `count` number of copies of the byte it was called on.
pub fn (b u8) repeat(count int) string {
	if count <= 0 {
		return ''
	} else if count == 1 {
		return b.ascii_str()
	}
	mut bytes := unsafe { malloc_noscan(count + 1) }
	unsafe {
		vmemset(bytes, b, count)
		bytes[count] = 0
	}
	return unsafe { bytes.vstring_with_len(count) }
}

// for atomic ints, internal
fn _Atomic__int_str(x int) string {
	return x.str()
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
