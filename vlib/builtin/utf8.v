// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

pub fn utf8_char_len(b byte) int { 
	return (( 0xe5000000 >> (( b >> 3 ) & 0x1e )) & 3 ) + 1 
}

// Convert utf32 to utf8
// utf32 == Codepoint
pub fn utf32_to_str(code u32) string {
	icode := int(code) 	//Prevents doing casts everywhere
  mut buffer := malloc(5)
	if icode <= 127 /* 0x7F */ {
		buffer[0] = icode
		return tos(buffer, 1)
	}
	if (icode <= 2047 /* 0x7FF */) {
		buffer[0] = 192 /*0xC0*/ | (icode >> 6)                   /* 110xxxxx */
		buffer[1] = 128 /*0x80*/ | (icode & 63 /*0x3F*/)          /* 10xxxxxx */
		return tos(buffer, 2)
	}
	if (icode <= 65535 /* 0xFFFF */) {
		buffer[0] = 224 /*0xE0*/ | (icode >> 12)                  /* 1110xxxx */
		buffer[1] = 128 /*0x80*/ | ((icode >> 6) & 63 /*0x3F*/)   /* 10xxxxxx */
		buffer[2] = 128 /*0x80*/ | (icode & 63 /*0x3F*/)          /* 10xxxxxx */
		return tos(buffer, 3)
	}
	if (icode <= 1114111 /* 0x10FFFF */) {
		buffer[0] = 240 /*0xF0*/ | (icode >> 18)                  /* 11110xxx */
		buffer[1] = 128 /*0x80*/ | ((icode >> 12) & 63 /*0x3F*/)  /* 10xxxxxx */
		buffer[2] = 128 /*0x80*/ | ((icode >> 6) & 63 /*0x3F*/)   /* 10xxxxxx */
		buffer[3] = 128 /*0x80*/ | (icode & 63 /*0x3F*/)          /* 10xxxxxx */
		return tos(buffer, 4)
	}
	return ''
}

// TODO copypasta
pub fn utf32_to_str_no_malloc(code u32, buf voidptr) string {
	icode := int(code) 	//Prevents doing casts everywhere
  mut buffer := byteptr(buf)
	if icode <= 127 /* 0x7F */ {
		buffer[0] = icode
		return tos(buffer, 1)
	}
	if (icode <= 2047 /* 0x7FF */) {
		buffer[0] = 192 /*0xC0*/ | (icode >> 6)                   /* 110xxxxx */
		buffer[1] = 128 /*0x80*/ | (icode & 63 /*0x3F*/)          /* 10xxxxxx */
		return tos(buffer, 2)
	}
	if (icode <= 65535 /* 0xFFFF */) {
		buffer[0] = 224 /*0xE0*/ | (icode >> 12)                  /* 1110xxxx */
		buffer[1] = 128 /*0x80*/ | ((icode >> 6) & 63 /*0x3F*/)   /* 10xxxxxx */
		buffer[2] = 128 /*0x80*/ | (icode & 63 /*0x3F*/)          /* 10xxxxxx */
		return tos(buffer, 3)
	}
	if (icode <= 1114111 /* 0x10FFFF */) {
		buffer[0] = 240 /*0xF0*/ | (icode >> 18)                  /* 11110xxx */
		buffer[1] = 128 /*0x80*/ | ((icode >> 12) & 63 /*0x3F*/)  /* 10xxxxxx */
		buffer[2] = 128 /*0x80*/ | ((icode >> 6) & 63 /*0x3F*/)   /* 10xxxxxx */
		buffer[3] = 128 /*0x80*/ | (icode & 63 /*0x3F*/)          /* 10xxxxxx */
		return tos(buffer, 4)
	}
	return ''
}

// Convert utf8 to utf32
pub fn (_rune string) utf32_code() int {
	if _rune.len == 0 {
		return 0
	}
	// save ASC symbol as is
	if _rune.len == 1 {
		return int(_rune[0])
	}
	mut b := byte(int(_rune[0]))
	// TODO should be
	// res := int( rune[0] << rune.len)
	b = b << _rune.len
	mut res := int(b)
	mut shift := 6 - _rune.len
	for i := 1; i < _rune.len; i++ {
		c := int(_rune[i])
		res = res << shift
		res |= c & 63 /* 0x3f */
		shift = 6
	}
	return res
}

