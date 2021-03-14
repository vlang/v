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

// TODO remove this once runes are implemented
pub fn (b []byte) bytestr() string {
	return bytes2string(b)
}

// TODO copy pasted from builder.v
fn bytes2string(b []byte) string {
	unsafe {
		buf := malloc(b.len + 1)
		C.memcpy(buf, b.data, b.len)
		buf[b.len] = 0
		return tos(buf, b.len)
	}
}
