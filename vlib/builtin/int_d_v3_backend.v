// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// The 128-bit integer types exist only in V3. V3 always selects this file for
// builtin, with its internal `v3_backend` define. The compatibility compiler, which
// still checks some legacy fixtures against this vlib, does not know these types.

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
	return buf[index..].bytestr()
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
