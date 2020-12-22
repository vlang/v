// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

/*
// old function for reference
pub fn (nn int) str1() string {
	mut n := nn
	if n == 0 {
		return '0'
	}
	max := 16
	mut buf := vcalloc(max + 1)
	mut len := 0
	mut is_neg := false
	if n < 0 {
		n = -n
		is_neg = true
	}
	// Fill the string from the end
	for n > 0 {
		d := n % 10
		buf[max - len - 1] = d + int(`0`)
		len++
		n = n / 10
	}
	// Prepend - if it's negative
	if is_neg {
		buf[max - len - 1] = `-`
		len++
	}
	buf[max] = `\0`
	return tos(buf + max - len, len)
}
*/
// ----- value to string functions -----
/*
// old function for reference
pub fn ptr_str(ptr voidptr) string {
	buf := malloc(sizeof(double) * 5 + 1) // TODO
	C.sprintf((buf), '%p', ptr)
	return tos(buf, vstrlen(buf))
}
*/
pub fn ptr_str(ptr voidptr) string {
	buf1 := u64(ptr).hex()
	return buf1
}

const (
	digit_pairs = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'
)

// This implementation is the quickest with gcc -O2
[inline]
pub fn (nn int) str_l(max int) string {
	mut n := i64(nn)
	mut d := 0
	if n == 0 {
		return '0'
	}
	mut buf := malloc(max + 1)
	mut is_neg := false
	if n < 0 {
		n = -n
		is_neg = true
	}
	mut index := max
	unsafe {
		buf[index--] = `\0`
	}
	for n > 0 {
		n1 := int(n / 100)
		d = ((int(n) - (n1 * 100)) << 1)
		n = n1
		unsafe {
			buf[index--] = digit_pairs.str[d++]
			buf[index--] = digit_pairs.str[d]
		}
	}
	index++
	// remove head zero
	if d < 20 {
		index++
	}
	// Prepend - if it's negative
	if is_neg {
		index--
		unsafe {
			buf[index] = `-`
		}
	}
	unsafe {
		C.memmove(buf, buf + index, (max - index) + 1)
		return tos(buf, (max - index))
	}
	// return tos(buf + index, (max-index))
}

pub fn (n i8) str() string {
	return int(n).str_l(5)
}

pub fn (n i16) str() string {
	return int(n).str_l(7)
}

pub fn (n u16) str() string {
	return int(n).str_l(7)
}

pub fn (n int) str() string {
	return n.str_l(12)
}

pub fn (nn u32) str() string {
	mut n := nn
	mut d := u32(0)
	if n == 0 {
		return '0'
	}
	max := 12
	mut buf := malloc(max + 1)
	mut index := max
	unsafe {
		buf[index--] = `\0`
	}
	for n > 0 {
		n1 := n / u32(100)
		d = ((n - (n1 * u32(100))) << u32(1))
		n = n1
		unsafe {
			buf[index--] = digit_pairs[d++]
			buf[index--] = digit_pairs[d]
		}
	}
	index++
	// remove head zero
	if d < u32(20) {
		index++
	}
	unsafe {
		C.memmove(buf, buf + index, (max - index) + 1)
		return tos(buf, (max - index))
	}
	// return tos(buf + index, (max-index))
}

[inline]
pub fn (n any_int) str() string {
	return i64(n).str()
}

pub fn (nn i64) str() string {
	mut n := nn
	mut d := i64(0)
	if n == 0 {
		return '0'
	}
	max := 20
	mut buf := vcalloc(max + 1)
	mut is_neg := false
	if n < 0 {
		n = -n
		is_neg = true
	}
	mut index := max
	unsafe {
		buf[index--] = `\0`
	}
	for n > 0 {
		n1 := n / i64(100)
		d = ((n - (n1 * i64(100))) << i64(1))
		n = n1
		unsafe {
			buf[index--] = digit_pairs[d++]
			buf[index--] = digit_pairs[d]
		}
	}
	index++
	// remove head zero
	if d < i64(20) {
		index++
	}
	// Prepend - if it's negative
	if is_neg {
		index--
		unsafe {
			buf[index] = `-`
		}
	}
	unsafe {
		C.memmove(buf, buf + index, (max - index) + 1)
		return tos(buf, (max - index))
	}
	// return tos(buf + index, (max-index))
}

pub fn (nn u64) str() string {
	mut n := nn
	mut d := u64(0)
	if n == 0 {
		return '0'
	}
	max := 20
	mut buf := vcalloc(max + 1)
	mut index := max
	unsafe {
		buf[index--] = `\0`
	}
	for n > 0 {
		n1 := n / 100
		d = ((n - (n1 * 100)) << 1)
		n = n1
		unsafe {
			buf[index--] = digit_pairs[d++]
			buf[index--] = digit_pairs[d]
		}
	}
	index++
	// remove head zero
	if d < 20 {
		index++
	}
	unsafe {
		C.memmove(buf, buf + index, (max - index) + 1)
		return tos(buf, (max - index))
	}
	// return tos(buf + index, (max-index))
}

pub fn (b bool) str() string {
	if b {
		return 'true'
	}
	return 'false'
}

// ----- value to hex string functions -----
/*
//old function for reference
pub fn (n int) hex1() string {
	len := if n >= 0 { n.str().len + 3 } else { 11 }
	hex := malloc(len) // 0x + \n
	count := C.sprintf((hex), '0x%x', n)
	return tos(hex, count)
}
*/
[inline]
fn u64_to_hex(nn u64, len byte) string {
	mut n := nn
	mut buf := [256]byte{}
	buf[len] = `\0`
	mut i := 0
	for i = len - 1; i >= 0; i-- {
		d := byte(n & 0xF)
		x := if d < 10 { d + `0` } else { d + 87 }
		buf[i] = x
		n = n >> 4
	}
	return string{
		str: memdup(buf, len + 1)
		len: len
	}
}

[inline]
fn u64_to_hex_no_leading_zeros(nn u64, len byte) string {
	mut n := nn
	mut buf := [256]byte{}
	buf[len] = `\0`
	mut i := 0
	for i = len - 1; i >= 0; i-- {
		d := byte(n & 0xF)
		x := if d < 10 { d + `0` } else { d + 87 }
		buf[i] = x
		n = n >> 4
		if n == 0 {
			break
		}
	}
	res_len := len - i
	return string{
		str: memdup(&buf[i], res_len + 1)
		len: res_len
	}
}

pub fn (nn byte) hex() string {
	if nn == 0 {
		return '00'
	}
	return u64_to_hex(nn, 2)
}

pub fn (nn i8) hex() string {
	return byte(nn).hex()
}

pub fn (nn u16) hex() string {
	if nn == 0 {
		return '0'
	}
	return u64_to_hex_no_leading_zeros(nn, 4)
}

pub fn (nn i16) hex() string {
	return u16(nn).hex()
}

pub fn (nn u32) hex() string {
	if nn == 0 {
		return '0'
	}
	return u64_to_hex_no_leading_zeros(nn, 8)
}

pub fn (nn int) hex() string {
	return u32(nn).hex()
}

pub fn (n int) hex2() string {
	return '0x' + n.hex()
}

pub fn (nn u64) hex() string {
	if nn == 0 {
		return '0'
	}
	return u64_to_hex_no_leading_zeros(nn, 16)
}

pub fn (nn i64) hex() string {
	return u64(nn).hex()
}

pub fn (nn any_int) hex() string {
	return u64(nn).hex()
}

pub fn (nn voidptr) str() string {
	return u64(nn).hex()
}

pub fn (nn byteptr) str() string {
	return u64(nn).hex()
}

// pub fn (nn byte) hex_full() string { return u64_to_hex(nn, 2) }
// pub fn (nn i8)  hex_full() string { return u64_to_hex(byte(nn), 2) }
// pub fn (nn u16) hex_full() string { return u64_to_hex(nn, 4) }
// pub fn (nn i16) hex_full() string { return u64_to_hex(u16(nn), 4) }
// pub fn (nn u32) hex_full() string { return u64_to_hex(nn, 8) }
// pub fn (nn int) hex_full() string { return u64_to_hex(u32(nn), 8) }
pub fn (nn u64) hex_full() string {
	return u64_to_hex(nn, 16)
}

// pub fn (nn i64) hex_full() string { return u64_to_hex(u64(nn), 16) }
// pub fn (nn any_int) hex_full() string { return u64_to_hex(nn, 16) }
// pub fn (nn voidptr) hex_full() string { return u64_to_hex(nn, 16) }
// pub fn (nn byteptr) hex_full() string { return u64_to_hex(nn, 16) }
pub fn (b byte) str() string {
	// TODO
	// return int(b).str_l(7)
	mut str := string{
		str: malloc(2)
		len: 1
	}
	unsafe {
		str.str[0] = b
		str.str[1] = `\0`
	}
	// println(str)
	return str
}

pub fn (b byte) str_escaped() string {
	str := match b {
		0 { '`\\' + '0`' } // Bug is preventing \\0 in a literal
		7 { '`\\a`' }
		8 { '`\\b`' }
		9 { '`\\t`' }
		10 { '`\\n`' }
		11 { '`\\v`' }
		12 { '`\\f`' }
		13 { '`\\r`' }
		32...126 { b.str() }
		else { '0x' + b.hex() }
	}
	return str
}
