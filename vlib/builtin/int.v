// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

pub fn ptr_str(ptr voidptr) string {
	buf := malloc(sizeof(double) * 5 + 1)// TODO
	C.sprintf(*char(buf), '%p', ptr)
	return tos(buf, vstrlen(buf))
}

// fn (nn i32) str() string {
// return i
// }
pub fn (nn int) str() string {
	mut n := nn
	if n == 0 {
		return '0'
	}
	max := 16
	mut buf := calloc(max)
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
	return tos(buf + max - len, len)
}

pub fn (nn u32) str() string {
	 mut n := nn
	if n == u32(0) {
		return '0'
	}
	max := 16
	mut buf := malloc(max)
	mut len := 0
	// Fill the string from the end
	for n > u32(0) {
		d := n % u32(10)
		buf[max - len - 1] = d + u32(`0`)
		len++
		n = n / u32(10)
	}
	return tos(buf + max - len, len)
}

/*
pub fn (nn byte) str() string {
	 mut n := nn
	if n == byte(0) {
		return '0'
	}
	max := 5
	mut buf := malloc(max)
	mut len := 0
	// Fill the string from the end
	for n > byte(0) {
		d := n % byte(10)
		buf[max - len - 1] = d + byte(`0`)
		len++
		n = n / byte(10)
	}
	return tos(buf + max - len, len)
}
*/

pub fn (nn i64) str() string {
	mut n := nn
	if n == i64(0) {
		return '0'
	}
	max := 32
	mut buf := malloc(max)
	mut len := 0
	mut is_neg := false
	if n < i64(0) {
		n = -n
		is_neg = true
	}
	// Fill the string from the end
	for n > i64(0) {
		d := int(n % i64(10))
		buf[max - len - 1] = d + int(`0`)
		len++
		n = n / i64(10)
	}
	// Prepend - if it's negative
	if is_neg {
		buf[max - len - 1] = `-`
		len++
	}
	return tos(buf + max - len, len)
}

pub fn (nn u64) str() string {
	 mut n := nn
	if n == u64(0) {
		return '0'
	}
	max := 32
	mut buf := malloc(max)
	mut len := 0
	// Fill the string from the end
	for n > u64(0) {
		d := n % u64(10)
		buf[max - len - 1] = d + u64(`0`)
		len++
		n = n / u64(10)
	}
	return tos(buf + max - len, len)
}

pub fn (b bool) str() string {
	if b {
		return 'true'
	}
	return 'false'
}

pub fn (n int) hex() string {
	len := if n >= 0 {
		n.str().len + 3
	} else {
		11
	}
	hex := malloc(len) // 0x + \n
	count := int(C.sprintf(*char(hex), '0x%x', n))
	return tos(hex, count)
}

pub fn (n i64) hex() string {
	len := if n >= i64(0) {
		n.str().len + 3
	} else {
		19
	}
	hex := malloc(len)
	count := int(C.sprintf(*char(hex), '0x%' C.PRIx64, n))
	return tos(hex, count)
}

pub fn (a []byte) contains(val byte) bool {
	for aa in a {
		if aa == val {
			return true
		}
	}
	return false
}

pub fn (c rune) str() string {
	fst_byte := int(c) >> 8 * 3 & 0xff
	len := utf8_char_len(fst_byte)
	mut str := string {
		len: len
		str: malloc(len + 1)
	}
	for i := 0; i < len; i++ {
		str.str[i] = int(c) >> 8 * (3 - i) & 0xff
	}
	str[len] = `\0`
	return str
}

pub fn (c byte) str() string {
	mut str := string {
		len: 1
		str: malloc(2)
	}
	str.str[0] = c
	str.str[1] = `\0`
	return str
}

pub fn (c byte) is_capital() bool {
	return c >= `A` && c <= `Z`
}

pub fn (b []byte) clone() []byte {
	mut res := [byte(0)].repeat(b.len)
	for i := 0; i < b.len; i++ {
		res[i] = b[i]
	}
	return res
}

