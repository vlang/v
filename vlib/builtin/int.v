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

const(
	digit_pairs = "00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999"
)

// This implementation is the quickest with gcc -O2
[inline]
pub fn (nn int) str_l(max int) string {
	mut n := nn
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
	buf[index--] = `\0`
	for n > 0 {
		n1 := n / 100
		d = ((n - (n1 * 100)) << 1)
		n = n1
		buf[index--] = digit_pairs.str[d++]
		buf[index--] = digit_pairs.str[d]
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

	C.memmove(buf,buf+index, (max-index)+1 )
	return tos(buf, (max-index))
	//return tos(buf + index, (max-index))
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
	buf[index--] = `\0`
	for n > 0 {
		n1 := n / u32(100)
		d = ((n - (n1 * u32(100))) << u32(1))
		n = n1
		buf[index--] = digit_pairs[d++]
		buf[index--] = digit_pairs[d]
	}
	index++

	// remove head zero
	if d < u32(20) {
		index++
	}

	C.memmove(buf,buf+index, (max-index)+1 )
	return tos(buf, (max-index))
	//return tos(buf + index, (max-index))
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
	buf[index--] = `\0`
	for n > 0 {
		n1 := n / i64(100)
		d = ((n - (n1 * i64(100))) << i64(1))
		n = n1
		buf[index--] = digit_pairs[d++]
		buf[index--] = digit_pairs[d]
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

	C.memmove(buf,buf+index, (max-index)+1 )
	return tos(buf, (max-index))
	//return tos(buf + index, (max-index))
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
	buf[index--] = `\0`
	for n > 0 {
		n1 := n / 100
		d = ((n - (n1 * 100)) << 1)
		n = n1
		buf[index--] = digit_pairs[d++]
		buf[index--] = digit_pairs[d]
	}
	index++

	// remove head zero
	if d < 20 {
		index++
	}

	C.memmove(buf,buf+index, (max-index)+1 )
	return tos(buf, (max-index))
	//return tos(buf + index, (max-index))
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

pub fn (nn byte) hex() string {
	if nn == 0 {
		return '0'
	}

	mut n := nn
	max := 2
	mut buf := malloc(max + 1)

	mut index := max
	buf[index--] = `\0`
	for n > 0 {
		d := n & 0xF
		n = n >> 4
		buf[index--] = if d < 10 { d + `0` } else { d + 87 }
	}
	//buf[index--] = `x`
	//buf[index]   = `0`
	index++

	return tos(buf + index, (max - index))
}

pub fn (nn i8) hex() string {
	return byte(nn).hex()
}

pub fn (nn u16) hex() string {
	if nn == 0 {
		return '0'
	}

	mut n := nn
	max := 5
	mut buf := malloc(max + 1)

	mut index := max
	buf[index--] = `\0`
	for n > 0 {
		d := byte(n & 0xF)
		n = n >> 4
		buf[index--] = if d < 10 { d + `0` } else { d + 87 }
	}
	//buf[index--] = `x`
	//buf[index]   = `0`
	index++

	return tos(buf + index, (max - index))
}

pub fn (nn i16) hex() string {
	return u16(nn).hex()
}

pub fn (nn u32) hex() string {
	if nn == 0 {
		return '0'
	}

	mut n := nn
	max := 10
	mut buf := malloc(max + 1)

	mut index := max
	buf[index--] = `\0`
	for n > 0 {
		d := byte(n & 0xF)
		n = n >> 4
		buf[index--] = if d < 10 { d + `0` } else { d + 87 }
	}
	//buf[index--] = `x`
	//buf[index]   = `0`
	index++

	return tos(buf + index, (max - index))
}

pub fn (nn int) hex() string {
	return u32(nn).hex()
}

pub fn (nn u64) hex() string {
	if nn == 0 {
		return '0'
	}

	mut n := nn
	max := 18
	mut buf := malloc(max + 1)

	mut index := max
	buf[index--] = `\0`
	for n > 0 {
		d := byte(n & 0xF)
		n = n >> 4
		buf[index--] = if d < 10 { d + `0` } else { d + 87 }
	}
	//buf[index--] = `x`
	//buf[index]   = `0`
	index++

	C.memmove(buf,buf+index, (max-index)+1 )
	return tos(buf, (max-index))
	//return tos(buf + index, (max-index))
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

// ----- utilities functions -----

pub fn (a []byte) contains(val byte) bool {
	for aa in a {
		if aa == val {
			return true
		}
	}
	return false
}


/*
pub fn (c rune) str() string {
	fst_byte := int(c)>>8 * 3 & 0xff
	len := utf8_char_len(fst_byte)
	mut str := string{
		len: len
		str: malloc(len + 1)
	}
	for i in 0..len {
		str.str[i] = int(c)>>8 * (3 - i) & 0xff
	}
	str.str[len] = `\0`
	return str
}
*/

pub fn (c byte) str() string {
	mut str := string{
		str: malloc(2)
		len: 1
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
	//mut res := make([]byte, {repeat:b.len})
	for i in 0..b.len {
		res[i] = b[i]
	}
	return res
}

