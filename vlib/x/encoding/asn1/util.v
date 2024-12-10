// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// ASN.1 Utility function

// read_bytes was safe version of bytes slicing, `src[pos..pos+size]`
fn read_bytes(src []u8, pos int, size int) ![]u8 {
	if src.len < 1 || pos > src.len || size > src.len {
		return error(' pos=${pos} or size=${size} bigger than len=${src.len}')
	}
	if pos + size > src.len {
		return error('pos + size maybe getting overflow')
	}

	ret := src[pos..pos + size]
	return ret
}

fn read_byte(src []u8, loc int) !(u8, int) {
	if src.len == 0 || loc + 1 > src.len {
		return error('invalid loc or len')
	}

	mut pos := loc
	result := src[pos]
	pos += 1

	return result, pos
}

fn read_digit(src []u8, loc int) !(u8, int) {
	val, pos := read_byte(src, loc)!
	// check its a digit, '0'-'9',
	// aka, 0x30 s/d 0x39 in hex, or 48-57 in dec
	if !val.is_digit() {
		return error('not digit byte')
	}

	digit := val - u8(0x30) // get the digit value
	return digit, pos
}

fn read_2_digits(src []u8, loc int) !(u8, int) {
	if loc >= src.len || src.len < 2 + loc {
		return error('not enough bytes')
	}
	mut val, mut pos := read_digit(src, loc)!

	first := val * 10

	if pos < src.len {
		val, pos = read_digit(src, pos)!
	}
	return first + val, pos
}

fn read_4_digits(src []u8, loc int) !(u16, int) {
	if loc >= src.len || src.len < 4 + loc {
		return error('not enough bytes')
	}
	mut val, mut pos := read_digit(src, loc)!
	first := u16(val) * 1000

	if pos < src.len {
		val, pos = read_digit(src, pos)!
	}
	second := u16(val) * 100

	if pos < src.len {
		val, pos = read_digit(src, pos)!
	}
	third := u16(val) * 10

	if pos < src.len {
		val, pos = read_digit(src, pos)!
	}
	fourth := u16(val)

	result := first + second + third + fourth
	return result, pos
}

fn validate_date(year u16, month u8, day u8) bool {
	if year < 0 {
		return false
	}
	if day < 1 {
		return false
	}
	mut dim := month
	match month {
		1, 3, 5, 7, 8, 10, 12 {
			dim = 31
		}
		4, 6, 9, 11 {
			dim = 30
		}
		2 {
			// kabisat
			if (year % 4 == 0 && year % 100 != 0) || year % 400 == 0 {
				dim = 29
			} else {
				dim = 28
			}
		}
		else {
			return false
		}
	}
	if day > dim {
		return false
	}

	return true
}
