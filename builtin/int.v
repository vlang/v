// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

fn (d double) str() string {
	buf := malloc(sizeof(double) * 5 + 1)// TODO
	C.sprintf(buf, '%f', d)
	return tos(buf, _strlen(buf))
}

fn (d float) str() string {
	buf := malloc(sizeof(double) * 5 + 1)// TODO
	C.sprintf(buf, '%f', d)
	return tos(buf, _strlen(buf))
}

fn (d f64) str() string {
	buf := malloc(sizeof(double) * 5 + 1)// TODO
	C.sprintf(buf, '%f', d)
	return tos(buf, _strlen(buf))
}

fn (d f32) str() string {
	buf := malloc(sizeof(double) * 5 + 1)// TODO
	C.sprintf(buf, '%f', d)
	return tos(buf, _strlen(buf))
}

fn ptr_str(ptr voidptr) string {
	buf := malloc(sizeof(double) * 5 + 1)// TODO
	C.sprintf(buf, '%p', ptr)
	return tos(buf, _strlen(buf))
}

// fn (nn i32) str() string {
// return i
// }
fn (nn int) str() string {
	mut n = nn
	if n == 0 {
		return '0'
	}
	max := 16
	mut buf := malloc(max)
	mut len := 0
	mut is_neg = false
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

fn (nn u8) str() string {
	mut n = nn
	if n == u8(0) {
		return '0'
	}
	max := 5
	mut buf := malloc(max)
	mut len := 0
	mut is_neg = false
	if n < u8(0) {
		n = -n
		is_neg = true
	}
	// Fill the string from the end
	for n > u8(0) {
		d := n % u8(10)
		buf[max - len - 1] = d + u8(`0`)
		len++
		n = n / u8(10)
	}
	// Prepend - if it's negative
	if is_neg {
		buf[max - len - 1] = `-`
		len++
	}
	return tos(buf + max - len, len)
}

fn (nn i64) str() string {
	mut n = nn
	if n == i64(0) {
		return '0'
	}
	max := 32
	mut buf := malloc(max)
	mut len := 0
	mut is_neg = false
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

fn (b bool) str() string {
	if b {
		return 'true'
	}
	return 'false'
}

fn (n int) hex() string {
	s := n.str()
	hex := malloc(s.len + 2)
	C.sprintf(hex, '0x%x', n)
	return tos(hex, s.len + 2)
}

fn (n i64) hex() string {
	s := n.str()
	hex := malloc(s.len + 2)
	C.sprintf(hex, '0x%x', n)
	return tos(hex, s.len + 2)
}

fn (a[]byte) contains(val byte) bool {
	for aa in a {
		if aa == val {
			return true
		}
	}
	return false
}

/*  TODO 
fn (c rune) str() string {
}
*/
fn (c byte) str() string {
	mut str := string {
		len: 1
		str: malloc(2)
	}
	str.str[0] = c
	str.str[1] = `\0`
	return str
}

