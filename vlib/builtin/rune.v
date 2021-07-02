// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module builtin

// This was never working correctly, the issue is now
// fixed however the type checks in checker need to be
// updated. if you uncomment it you will see the issue
// type rune = int

pub fn (c rune) str() string {
	return utf32_to_str(u32(c))
	/*
	unsafe {
		fst_byte := int(c)>>8 * 3 & 0xff
		len := utf8_char_len(byte(fst_byte))
		println('len=$len')
		mut str := string{
			len: len
			str: malloc(len + 1)
		}
		for i in 0..len {
			str.str[i] = byte(int(c)>>8 * (3 - i) & 0xff)
		}
		str.str[len] = `\0`
		println(str)
		return str
	}
	*/
}

// string converts a rune array to a string
pub fn (ra []rune) string() string {
	mut res := ''
	for r in ra {
		res += r.str()
	}
	return res
}

// Define this on byte as well, so that we can do `s[0].is_capital()`
pub fn (c byte) is_capital() bool {
	return c >= `A` && c <= `Z`
}

pub fn (b []byte) clone() []byte {
	mut res := []byte{len: b.len}
	// mut res := make([]byte, {repeat:b.len})
	for i in 0 .. b.len {
		res[i] = b[i]
	}
	return res
}

// TODO: remove this once runes are implemented
pub fn (b []byte) bytestr() string {
	unsafe {
		buf := malloc_noscan(b.len + 1)
		C.memcpy(buf, b.data, b.len)
		buf[b.len] = 0
		return tos(buf, b.len)
	}
}

// is_space returns `true` if the byte is a white space character.
// The following list is considered white space characters: ` `, `\t`, `\n`, `\v`, `\f`, `\r`, 0x85, 0xa0
// Example: assert byte(` `).is_space() == true
[inline]
pub fn (c rune) is_space() bool {
	// 0x85 is NEXT LINE (NEL)
	// 0xa0 is NO-BREAK SPACE
	return c == 32 || (c > 8 && c < 14) || (c == 0x85) || (c == 0xa0)
}

// is_digit returns `true` if the byte is in range 0-9 and `false` otherwise.
// Example: assert byte(`9`) == true
[inline]
pub fn (c rune) is_digit() bool {
	return c >= `0` && c <= `9`
}

// is_hex_digit returns `true` if the byte is either in range 0-9, a-f or A-F and `false` otherwise.
// Example: assert byte(`F`) == true
[inline]
pub fn (c rune) is_hex_digit() bool {
	return c.is_digit() || (c >= `a` && c <= `f`) || (c >= `A` && c <= `F`)
}

// is_oct_digit returns `true` if the byte is in range 0-7 and `false` otherwise.
// Example: assert byte(`7`) == true
[inline]
pub fn (c rune) is_oct_digit() bool {
	return c >= `0` && c <= `7`
}

// is_bin_digit returns `true` if the byte is a binary digit (0 or 1) and `false` otherwise.
// Example: assert byte(`0`) == true
[inline]
pub fn (c rune) is_bin_digit() bool {
	return c == `0` || c == `1`
}

// is_letter returns `true` if the byte is in range a-z or A-Z and `false` otherwise.
// Example: assert byte(`V`) == true
[inline]
pub fn (c rune) is_letter() bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
}
