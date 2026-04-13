// utf-8 utility string functions
//
// Copyright (c) 2019-2024 Dario Deledda. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module utf8

// Utility functions

// len return the length as number of unicode chars from a string
pub fn len(s string) int {
	if s.len == 0 {
		return 0
	}

	mut count := 0
	mut index := 0

	for {
		ch_len := utf8_char_len(s[index])
		index += ch_len
		count++
		if index >= s.len {
			break
		}
	}
	return count
}

// get_rune convert a UTF-8 unicode codepoint in string[index] into a UTF-32 encoded rune
pub fn get_rune(s string, index int) rune {
	mut res := 0
	mut ch_len := 0
	if s.len > 0 {
		ch_len = utf8_char_len(s[index])

		if ch_len == 1 {
			return u16(s[index])
		}
		if ch_len > 1 && ch_len < 5 {
			mut lword := 0
			for i := 0; i < ch_len; i++ {
				lword = int(u32(lword) << 8 | u32(s[index + i]))
			}

			// 2 byte utf-8
			// byte format: 110xxxxx 10xxxxxx
			//
			if ch_len == 2 {
				res = (lword & 0x1f00) >> 2 | (lword & 0x3f)
			}
			// 3 byte utf-8
			// byte format: 1110xxxx 10xxxxxx 10xxxxxx
			//
			else if ch_len == 3 {
				res = (lword & 0x0f0000) >> 4 | (lword & 0x3f00) >> 2 | (lword & 0x3f)
			}
			// 4 byte utf-8
			// byte format: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
			//
			else if ch_len == 4 {
				res = ((lword & 0x07000000) >> 6) | ((lword & 0x003f0000) >> 4) | ((lword & 0x00003F00) >> 2) | (lword & 0x0000003f)
			}
		}
	}
	return res
}

// raw_index - get the raw unicode character from the UTF-8 string by the given index value as UTF-8 string.
// example: utf8.raw_index('我是V Lang', 1) => '是'
pub fn raw_index(s string, index int) string {
	mut r := []rune{}

	for i := 0; i < s.len; i++ {
		if r.len - 1 == index {
			break
		}

		b := s[i]
		ch_len := int((u32(0xe5000000) >> ((b >> 3) & 0x1e)) & 3)

		r << if ch_len > 0 {
			i += ch_len
			rune(get_rune(s, i - ch_len))
		} else {
			rune(b)
		}
	}

	return r[index].str()
}

// reverse - returns a reversed string.
// example: utf8.reverse('你好世界hello world') => 'dlrow olleh界世好你'.
pub fn reverse(s string) string {
	len_s := len(s)
	if len_s == 0 || len_s == 1 {
		return s.clone()
	}
	mut str_array := []string{}
	for i in 0 .. len_s {
		str_array << raw_index(s, i)
	}
	str_array = str_array.reverse()
	return str_array.join('')
}

// Conversion functions

// to_upper return an uppercase string from a string
pub fn to_upper(s string) string {
	return convert_case(s, true)
}

// to_lower return an lowercase string from a string
pub fn to_lower(s string) string {
	return convert_case(s, false)
}

// Punctuation functions
//
// The "western" function search on a small table, that is quicker than
// the global unicode table search. **Use only for western chars**.

// Western

// is_punct return true if the string[index] byte is the start of a unicode western punctuation
pub fn is_punct(s string, index int) bool {
	return is_rune_punct(get_rune(s, index))
}

// is_control return true if the rune is control code
pub fn is_control(r rune) bool {
	// control codes are all below 0xff
	if r > max_latin_1 {
		return false
	}
	return props[u8(r)] == 1
}

// is_letter returns true if the rune is unicode letter or in unicode category L
pub fn is_letter(r rune) bool {
	if (r >= `a` && r <= `z`) || (r >= `A` && r <= `Z`) {
		return true
	} else if r <= max_latin_1 {
		return props[u8(r)] & p_l_mask != 0
	}
	return is_excluding_latin(letter_table, r)
}

// is_space returns true if the rune is character in unicode category Z with property white space or the following character set:
// ```
// `\t`, `\n`, `\v`, `\f`, `\r`, ` `, 0x85 (NEL), 0xA0 (NBSP)
// ```
pub fn is_space(r rune) bool {
	if r <= max_latin_1 {
		match r {
			`\t`, `\n`, `\v`, `\f`, `\r`, ` `, 0x85, 0xA0 {
				return true
			}
			else {
				return false
			}
		}
	}
	return is_excluding_latin(white_space_table, r)
}

// is_number returns true if the rune is unicode number or in unicode category N
pub fn is_number(r rune) bool {
	if r <= max_latin_1 {
		return props[u8(r)] & p_n != 0
	}
	return is_excluding_latin(number_table, r)
}

// is_rune_punct return true if the input unicode is a western unicode punctuation
pub fn is_rune_punct(r rune) bool {
	return find_punct_in_table(r, unicode_punct_western) != rune(-1)
}

// Global

// is_global_punct return true if the string[index] byte of is the start of a global unicode punctuation
pub fn is_global_punct(s string, index int) bool {
	return is_rune_global_punct(get_rune(s, index))
}

// is_rune_global_punct return true if the input unicode is a global unicode punctuation
pub fn is_rune_global_punct(r rune) bool {
	return find_punct_in_table(r, unicode_punct) != rune(-1)
}

// Private functions

// utf8_to_lower raw utf-8 to_lower function
fn utf8_to_lower(in_cp int) int {
	mut cp := in_cp
	if (0x0041 <= cp && 0x005a >= cp) || (0x00c0 <= cp && 0x00d6 >= cp)
		|| (0x00d8 <= cp && 0x00de >= cp) || (0x0391 <= cp && 0x03a1 >= cp)
		|| (0x03a3 <= cp && 0x03ab >= cp) || (0x0410 <= cp && 0x042f >= cp) {
		cp += 32
	} else if 0x0400 <= cp && 0x040f >= cp {
		cp += 80
	} else if (0x0100 <= cp && 0x012f >= cp) || (0x0132 <= cp && 0x0137 >= cp)
		|| (0x014a <= cp && 0x0177 >= cp) || (0x0182 <= cp && 0x0185 >= cp)
		|| (0x01a0 <= cp && 0x01a5 >= cp) || (0x01de <= cp && 0x01ef >= cp)
		|| (0x01f8 <= cp && 0x021f >= cp) || (0x0222 <= cp && 0x0233 >= cp)
		|| (0x0246 <= cp && 0x024f >= cp) || (0x03d8 <= cp && 0x03ef >= cp)
		|| (0x0460 <= cp && 0x0481 >= cp) || (0x048a <= cp && 0x04ff >= cp) {
		cp |= 0x1
	} else if (0x0139 <= cp && 0x0148 >= cp) || (0x0179 <= cp && 0x017e >= cp)
		|| (0x01af <= cp && 0x01b0 >= cp) || (0x01b3 <= cp && 0x01b6 >= cp)
		|| (0x01cd <= cp && 0x01dc >= cp) {
		cp += 1
		cp &= ~0x1
	} else if (0x0531 <= cp && 0x0556 >= cp) || (0x10A0 <= cp && 0x10C5 >= cp) {
		// ARMENIAN or GEORGIAN
		cp += 0x30
	} else if ((0x1E00 <= cp && 0x1E94 >= cp) || (0x1EA0 <= cp && 0x1EF8 >= cp)) && cp & 1 == 0 {
		// LATIN CAPITAL LETTER
		cp += 1
	} else if 0x24B6 <= cp && 0x24CF >= cp {
		// CIRCLED LATIN
		cp += 0x1a
	} else if 0xFF21 <= cp && 0xFF3A >= cp {
		// FULLWIDTH LATIN CAPITAL
		cp += 0x19
	} else if (0x1F08 <= cp && 0x1F0F >= cp) || (0x1F18 <= cp && 0x1F1D >= cp)
		|| (0x1F28 <= cp && 0x1F2F >= cp) || (0x1F38 <= cp && 0x1F3F >= cp)
		|| (0x1F48 <= cp && 0x1F4D >= cp) || (0x1F68 <= cp && 0x1F6F >= cp)
		|| (0x1F88 <= cp && 0x1F8F >= cp) || (0x1F98 <= cp && 0x1F9F >= cp)
		|| (0x1FA8 <= cp && 0x1FAF >= cp) {
		// GREEK
		cp -= 8
	} else {
		match cp {
			0x0178 { cp = 0x00ff }
			0x0243 { cp = 0x0180 }
			0x018e { cp = 0x01dd }
			0x023d { cp = 0x019a }
			0x0220 { cp = 0x019e }
			0x01b7 { cp = 0x0292 }
			0x01c4 { cp = 0x01c6 }
			0x01c7 { cp = 0x01c9 }
			0x01ca { cp = 0x01cc }
			0x01f1 { cp = 0x01f3 }
			0x01f7 { cp = 0x01bf }
			0x0187 { cp = 0x0188 }
			0x018b { cp = 0x018c }
			0x0191 { cp = 0x0192 }
			0x0198 { cp = 0x0199 }
			0x01a7 { cp = 0x01a8 }
			0x01ac { cp = 0x01ad }
			0x01af { cp = 0x01b0 }
			0x01b8 { cp = 0x01b9 }
			0x01bc { cp = 0x01bd }
			0x01f4 { cp = 0x01f5 }
			0x023b { cp = 0x023c }
			0x0241 { cp = 0x0242 }
			0x03fd { cp = 0x037b }
			0x03fe { cp = 0x037c }
			0x03ff { cp = 0x037d }
			0x037f { cp = 0x03f3 }
			0x0386 { cp = 0x03ac }
			0x0388 { cp = 0x03ad }
			0x0389 { cp = 0x03ae }
			0x038a { cp = 0x03af }
			0x038c { cp = 0x03cc }
			0x038e { cp = 0x03cd }
			0x038f { cp = 0x03ce }
			0x0370 { cp = 0x0371 }
			0x0372 { cp = 0x0373 }
			0x0376 { cp = 0x0377 }
			0x03f4 { cp = 0x03b8 }
			0x03cf { cp = 0x03d7 }
			0x03f9 { cp = 0x03f2 }
			0x03f7 { cp = 0x03f8 }
			0x03fa { cp = 0x03fb }
			// GREEK
			0x1F59 { cp = 0x1F51 }
			0x1F5B { cp = 0x1F53 }
			0x1F5D { cp = 0x1F55 }
			0x1F5F { cp = 0x1F57 }
			0x1FB8 { cp = 0x1FB0 }
			0x1FB9 { cp = 0x1FB1 }
			0x1FD8 { cp = 0x1FD0 }
			0x1FD9 { cp = 0x1FD1 }
			0x1FE8 { cp = 0x1FE0 }
			0x1FE9 { cp = 0x1FE1 }
			else {}
		}
	}

	return cp
}

// utf8_to_upper raw utf-8 to_upper function
fn utf8_to_upper(in_cp int) int {
	mut cp := in_cp
	if (0x0061 <= cp && 0x007a >= cp) || (0x00e0 <= cp && 0x00f6 >= cp)
		|| (0x00f8 <= cp && 0x00fe >= cp) || (0x03b1 <= cp && 0x03c1 >= cp)
		|| (0x03c3 <= cp && 0x03cb >= cp) || (0x0430 <= cp && 0x044f >= cp) {
		cp -= 32
	} else if 0x0450 <= cp && 0x045f >= cp {
		cp -= 80
	} else if (0x0100 <= cp && 0x012f >= cp) || (0x0132 <= cp && 0x0137 >= cp)
		|| (0x014a <= cp && 0x0177 >= cp) || (0x0182 <= cp && 0x0185 >= cp)
		|| (0x01a0 <= cp && 0x01a5 >= cp) || (0x01de <= cp && 0x01ef >= cp)
		|| (0x01f8 <= cp && 0x021f >= cp) || (0x0222 <= cp && 0x0233 >= cp)
		|| (0x0246 <= cp && 0x024f >= cp) || (0x03d8 <= cp && 0x03ef >= cp)
		|| (0x0460 <= cp && 0x0481 >= cp) || (0x048a <= cp && 0x04ff >= cp) {
		cp &= ~0x1
	} else if (0x0139 <= cp && 0x0148 >= cp) || (0x0179 <= cp && 0x017e >= cp)
		|| (0x01af <= cp && 0x01b0 >= cp) || (0x01b3 <= cp && 0x01b6 >= cp)
		|| (0x01cd <= cp && 0x01dc >= cp) {
		cp -= 1
		cp |= 0x1
	} else if (0x0561 <= cp && 0x0586 >= cp) || (0x10D0 <= cp && 0x10F5 >= cp) {
		// ARMENIAN or GEORGIAN
		cp -= 0x30
	} else if ((0x1E01 <= cp && 0x1E95 >= cp) || (0x1EA1 <= cp && 0x1EF9 >= cp)) && cp & 1 == 1 {
		// LATIN CAPITAL LETTER
		cp -= 1
	} else if 0x24D0 <= cp && 0x24E9 >= cp {
		// CIRCLED LATIN
		cp -= 0x1a
	} else if 0xFF41 <= cp && 0xFF5A >= cp {
		// FULLWIDTH LATIN CAPITAL
		cp -= 0x19
	} else if (0x1F00 <= cp && 0x1F07 >= cp) || (0x1F10 <= cp && 0x1F15 >= cp)
		|| (0x1F20 <= cp && 0x1F27 >= cp) || (0x1F30 <= cp && 0x1F37 >= cp)
		|| (0x1F40 <= cp && 0x1F45 >= cp) || (0x1F60 <= cp && 0x1F67 >= cp)
		|| (0x1F80 <= cp && 0x1F87 >= cp) || (0x1F90 <= cp && 0x1F97 >= cp)
		|| (0x1FA0 <= cp && 0x1FA7 >= cp) {
		// GREEK
		cp += 8
	} else {
		match cp {
			0x00ff { cp = 0x0178 }
			0x0180 { cp = 0x0243 }
			0x01dd { cp = 0x018e }
			0x019a { cp = 0x023d }
			0x019e { cp = 0x0220 }
			0x0292 { cp = 0x01b7 }
			0x01c6 { cp = 0x01c4 }
			0x01c9 { cp = 0x01c7 }
			0x01cc { cp = 0x01ca }
			0x01f3 { cp = 0x01f1 }
			0x01bf { cp = 0x01f7 }
			0x0188 { cp = 0x0187 }
			0x018c { cp = 0x018b }
			0x0192 { cp = 0x0191 }
			0x0199 { cp = 0x0198 }
			0x01a8 { cp = 0x01a7 }
			0x01ad { cp = 0x01ac }
			0x01b0 { cp = 0x01af }
			0x01b9 { cp = 0x01b8 }
			0x01bd { cp = 0x01bc }
			0x01f5 { cp = 0x01f4 }
			0x023c { cp = 0x023b }
			0x0242 { cp = 0x0241 }
			0x037b { cp = 0x03fd }
			0x037c { cp = 0x03fe }
			0x037d { cp = 0x03ff }
			0x03f3 { cp = 0x037f }
			0x03ac { cp = 0x0386 }
			0x03ad { cp = 0x0388 }
			0x03ae { cp = 0x0389 }
			0x03af { cp = 0x038a }
			0x03cc { cp = 0x038c }
			0x03cd { cp = 0x038e }
			0x03ce { cp = 0x038f }
			0x0371 { cp = 0x0370 }
			0x0373 { cp = 0x0372 }
			0x0377 { cp = 0x0376 }
			0x03d1 { cp = 0x0398 }
			0x03d7 { cp = 0x03cf }
			0x03f2 { cp = 0x03f9 }
			0x03f8 { cp = 0x03f7 }
			0x03fb { cp = 0x03fa }
			// GREEK
			0x1F51 { cp = 0x1F59 }
			0x1F53 { cp = 0x1F5B }
			0x1F55 { cp = 0x1F5D }
			0x1F57 { cp = 0x1F5F }
			0x1FB0 { cp = 0x1FB8 }
			0x1FB1 { cp = 0x1FB9 }
			0x1FD0 { cp = 0x1FD8 }
			0x1FD1 { cp = 0x1FD9 }
			0x1FE0 { cp = 0x1FE8 }
			0x1FE1 { cp = 0x1FE9 }
			else {}
		}
	}

	return cp
}

// convert_case converts letter cases
//
// if upper_flag == true  then convert lowercase ==> uppercase
// if upper_flag == false then convert uppercase ==> lowercase
@[direct_array_access]
fn convert_case(s string, upper_flag bool) string {
	mut index := 0
	mut tab_char := 0
	mut str_res := unsafe { malloc_noscan(s.len + 1) }

	for {
		ch_len := utf8_char_len(s[index])

		if ch_len == 1 {
			if upper_flag == true {
				unsafe {
					// Subtract 0x20 from ASCII lowercase to convert to uppercase.
					c := s[index]
					str_res[index] = if c >= 0x61 && c <= 0x7a { c & 0xdf } else { c }
				}
			} else {
				unsafe {
					// Add 0x20 to ASCII uppercase to convert to lowercase.
					c := s[index]
					str_res[index] = if c >= 0x41 && c <= 0x5a { c | 0x20 } else { c }
				}
			}
		} else if ch_len > 1 && ch_len < 5 {
			mut lword := 0

			for i := 0; i < ch_len; i++ {
				lword = int(u32(lword) << 8 | u32(s[index + i]))
			}

			// println("#${index} (${lword})")

			mut res := 0

			// 2 byte utf-8
			// byte format: 110xxxxx 10xxxxxx
			//
			if ch_len == 2 {
				res = (lword & 0x1f00) >> 2 | (lword & 0x3f)
			}
			// 3 byte utf-8
			// byte format: 1110xxxx 10xxxxxx 10xxxxxx
			//
			else if ch_len == 3 {
				res = (lword & 0x0f0000) >> 4 | (lword & 0x3f00) >> 2 | (lword & 0x3f)
			}
			// 4 byte utf-8
			// byte format: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
			//
			else if ch_len == 4 {
				res = ((lword & 0x07000000) >> 6) | ((lword & 0x003f0000) >> 4) | ((lword & 0x00003F00) >> 2) | (lword & 0x0000003f)
			}

			// println("res: ${res.hex():8}")

			if upper_flag == false {
				tab_char = utf8_to_lower(res)
			} else {
				tab_char = utf8_to_upper(res)
			}

			if ch_len == 2 {
				ch0 := u8((tab_char >> 6) & 0x1f) | 0xc0 // 110x xxxx
				ch1 := u8((tab_char >> 0) & 0x3f) | 0x80 // 10xx xxxx
				// C.printf("[%02x%02x] \n",ch0,ch1)

				unsafe {
					str_res[index + 0] = ch0
					str_res[index + 1] = ch1
				}
				///***************************************************************
				//  BUG: doesn't compile, workaround use shitf to right of 0 bit
				///***************************************************************
				// str_res[index + 1 ] = u8( tab_char & 0xbf )	// 1011 1111
			} else if ch_len == 3 {
				ch0 := u8((tab_char >> 12) & 0x0f) | 0xe0 // 1110 xxxx
				ch1 := u8((tab_char >> 6) & 0x3f) | 0x80 // 10xx xxxx
				ch2 := u8((tab_char >> 0) & 0x3f) | 0x80 // 10xx xxxx
				// C.printf("[%02x%02x%02x] \n",ch0,ch1,ch2)

				unsafe {
					str_res[index + 0] = ch0
					str_res[index + 1] = ch1
					str_res[index + 2] = ch2
				}
			}
			// TODO: write if needed
			else if ch_len == 4 {
				// place holder!!
				// at the present time simply copy the utf8 char
				for i in 0 .. ch_len {
					unsafe {
						str_res[index + i] = s[index + i]
					}
				}
			}
		} else {
			// other cases, just copy the string
			for i in 0 .. ch_len {
				unsafe {
					str_res[index + i] = s[index + i]
				}
			}
		}

		index += ch_len

		// we are done, exit the loop
		if index >= s.len {
			break
		}
	}

	// for c compatibility set the ending 0
	unsafe {
		str_res[index] = 0
		return tos(str_res, s.len)
	}
}

// find_punct_in_table looks for valid punctuation in table
@[direct_array_access]
fn find_punct_in_table(in_code rune, in_table []rune) rune {
	// uses simple binary search

	mut first_index := 0
	mut last_index := (in_table.len)
	mut index := 0
	mut x := rune(0)

	for {
		x = in_table[index]
		// C.printf("(%d..%d) index:%d base[%08x]==>[%08x]\n",first_index,last_index,index,in_code,x)

		if x == in_code {
			return index
		} else if x > in_code {
			last_index = index
		} else {
			first_index = index
		}

		if (last_index - first_index) <= 1 {
			break
		}
		index = (first_index + last_index) >> 1
	}

	return -1
}

// Unicode punctuation chars
//
// source: http://www.unicode.org/faq/punctuation_symbols.html

// Western punctuation mark
// Character	Name	Browser	Image
const unicode_punct_western = [
	rune(0x0021), // EXCLAMATION MARK !
	0x0022, // QUOTATION MARK "
	0x0027, // APOSTROPHE '
	0x002A, // ASTERISK *
	0x002C, // COMMA ,
	0x002E, // FULL STOP	.
	0x002F, // SOLIDUS /
	0x003A, // COLON :
	0x003B, // SEMICOLON	;
	0x003F, // QUESTION MARK ?
	0x00A1, // INVERTED EXCLAMATION MARK ¡
	0x00A7, // SECTION SIGN	§
	0x00B6, // PILCROW SIGN	¶
	0x00B7, // MIDDLE DOT ·
	0x00BF, // INVERTED QUESTION MARK ¿
	0x037E, // GREEK QUESTION MARK ;
	0x0387, // GREEK ANO TELEIA ·
	0x055A, // ARMENIAN APOSTROPHE ՚
	0x055B, // ARMENIAN EMPHASIS MARK ՛
	0x055C, // ARMENIAN EXCLAMATION MARK ՜
	0x055D, // ARMENIAN COMMA	՝
	0x055E, // ARMENIAN QUESTION MARK ՞
	0x055F, // ARMENIAN ABBREVIATION MARK	՟
	0x0589, // ARMENIAN FULL STOP	։
	0x05C0, // HEBREW PUNCTUATION PASEQ	׀
	0x05C3, // HEBREW PUNCTUATION SOF PASUQ	׃
	0x05C6, // HEBREW PUNCTUATION NUN HAFUKHA	׆
	0x05F3, // HEBREW PUNCTUATION GERESH	׳
	0x05F4, // HEBREW PUNCTUATION GERSHAYIM	״
]

// Unicode Characters in the 'Punctuation, Other' Category
// Character	Name	Browser	Image
const unicode_punct = [
	rune(0x0021), // EXCLAMATION MARK	!
	0x0022, // QUOTATION MARK	"
	0x0023, // NUMBER SIGN	#
	0x0025, // PERCENT SIGN	%
	0x0026, // AMPERSAND	&
	0x0027, // APOSTROPHE	'
	0x002A, // ASTERISK	*
	0x002C, // COMMA	,
	0x002E, // FULL STOP	.
	0x002F, // SOLIDUS	/
	0x003A, // COLON	:
	0x003B, // SEMICOLON	;
	0x003F, // QUESTION MARK	?
	0x0040, // COMMERCIAL AT	@
	0x005C, // REVERSE SOLIDUS	\
	0x00A1, // INVERTED EXCLAMATION MARK	¡
	0x00A7, // SECTION SIGN	§
	0x00B6, // PILCROW SIGN	¶
	0x00B7, // MIDDLE DOT	·
	0x00BF, // INVERTED QUESTION MARK	¿
	0x037E, // GREEK QUESTION MARK	;
	0x0387, // GREEK ANO TELEIA	·
	0x055A, // ARMENIAN APOSTROPHE	՚
	0x055B, // ARMENIAN EMPHASIS MARK	՛
	0x055C, // ARMENIAN EXCLAMATION MARK	՜
	0x055D, // ARMENIAN COMMA	՝
	0x055E, // ARMENIAN QUESTION MARK	՞
	0x055F, // ARMENIAN ABBREVIATION MARK	՟
	0x0589, // ARMENIAN FULL STOP	։
	0x05C0, // HEBREW PUNCTUATION PASEQ	׀
	0x05C3, // HEBREW PUNCTUATION SOF PASUQ	׃
	0x05C6, // HEBREW PUNCTUATION NUN HAFUKHA	׆
	0x05F3, // HEBREW PUNCTUATION GERESH	׳
	0x05F4, // HEBREW PUNCTUATION GERSHAYIM	״
	0x0609, // ARABIC-INDIC PER MILLE SIGN	؉
	0x060A, // ARABIC-INDIC PER TEN THOUSAND SIGN	؊
	0x060C, // ARABIC COMMA	،
	0x060D, // ARABIC DATE SEPARATOR	؍
	0x061B, // ARABIC SEMICOLON	؛
	0x061E, // ARABIC TRIPLE DOT PUNCTUATION MARK	؞
	0x061F, // ARABIC QUESTION MARK	؟
	0x066A, // ARABIC PERCENT SIGN	٪
	0x066B, // ARABIC DECIMAL SEPARATOR	٫
	0x066C, // ARABIC THOUSANDS SEPARATOR	٬
	0x066D, // ARABIC FIVE POINTED STAR	٭
	0x06D4, // ARABIC FULL STOP	۔
	0x0700, // SYRIAC END OF PARAGRAPH	܀
	0x0701, // SYRIAC SUPRALINEAR FULL STOP	܁
	0x0702, // SYRIAC SUBLINEAR FULL STOP	܂
	0x0703, // SYRIAC SUPRALINEAR COLON	܃
	0x0704, // SYRIAC SUBLINEAR COLON	܄
	0x0705, // SYRIAC HORIZONTAL COLON	܅
	0x0706, // SYRIAC COLON SKEWED LEFT	܆
	0x0707, // SYRIAC COLON SKEWED RIGHT	܇
	0x0708, // SYRIAC SUPRALINEAR COLON SKEWED LEFT	܈
	0x0709, // SYRIAC SUBLINEAR COLON SKEWED RIGHT	܉
	0x070A, // SYRIAC CONTRACTION	܊
	0x070B, // SYRIAC HARKLEAN OBELUS	܋
	0x070C, // SYRIAC HARKLEAN METOBELUS	܌
	0x070D, // SYRIAC HARKLEAN ASTERISCUS	܍
	0x07F7, // NKO SYMBOL GBAKURUNEN	߷
	0x07F8, // NKO COMMA	߸
	0x07F9, // NKO EXCLAMATION MARK	߹
	0x0830, // SAMARITAN PUNCTUATION NEQUDAA	࠰
	0x0831, // SAMARITAN PUNCTUATION AFSAAQ	࠱
	0x0832, // SAMARITAN PUNCTUATION ANGED	࠲
	0x0833, // SAMARITAN PUNCTUATION BAU	࠳
	0x0834, // SAMARITAN PUNCTUATION ATMAAU	࠴
	0x0835, // SAMARITAN PUNCTUATION SHIYYAALAA	࠵
	0x0836, // SAMARITAN ABBREVIATION MARK	࠶
	0x0837, // SAMARITAN PUNCTUATION MELODIC QITSA	࠷
	0x0838, // SAMARITAN PUNCTUATION ZIQAA	࠸
	0x0839, // SAMARITAN PUNCTUATION QITSA	࠹
	0x083A, // SAMARITAN PUNCTUATION ZAEF	࠺
	0x083B, // SAMARITAN PUNCTUATION TURU	࠻
	0x083C, // SAMARITAN PUNCTUATION ARKAANU	࠼
	0x083D, // SAMARITAN PUNCTUATION SOF MASHFAAT	࠽
	0x083E, // SAMARITAN PUNCTUATION ANNAAU	࠾
	0x085E, // MANDAIC PUNCTUATION	࡞
	0x0964, // DEVANAGARI DANDA	।
	0x0965, // DEVANAGARI DOUBLE DANDA	॥
	0x0970, // DEVANAGARI ABBREVIATION SIGN	॰
	0x09FD, // BENGALI ABBREVIATION SIGN	৽
	0x0A76, // GURMUKHI ABBREVIATION SIGN	੶
	0x0AF0, // GUJARATI ABBREVIATION SIGN	૰
	0x0C77, // TELUGU SIGN SIDDHAM	౷
	0x0C84, // KANNADA SIGN SIDDHAM	಄
	0x0DF4, // SINHALA PUNCTUATION KUNDDALIYA	෴
	0x0E4F, // THAI CHARACTER FONGMAN	๏
	0x0E5A, // THAI CHARACTER ANGKHANKHU	๚
	0x0E5B, // THAI CHARACTER KHOMUT	๛
	0x0F04, // TIBETAN MARK INITIAL YIG MGO MDUN MA	༄
	0x0F05, // TIBETAN MARK CLOSING YIG MGO SGAB MA	༅
	0x0F06, // TIBETAN MARK CARET YIG MGO PHUR SHAD MA	༆
	0x0F07, // TIBETAN MARK YIG MGO TSHEG SHAD MA	༇
	0x0F08, // TIBETAN MARK SBRUL SHAD	༈
	0x0F09, // TIBETAN MARK BSKUR YIG MGO	༉
	0x0F0A, // TIBETAN MARK BKA- SHOG YIG MGO	༊
	0x0F0B, // TIBETAN MARK INTERSYLLABIC TSHEG	་
	0x0F0C, // TIBETAN MARK DELIMITER TSHEG BSTAR	༌
	0x0F0D, // TIBETAN MARK SHAD	།
	0x0F0E, // TIBETAN MARK NYIS SHAD	༎
	0x0F0F, // TIBETAN MARK TSHEG SHAD	༏
	0x0F10, // TIBETAN MARK NYIS TSHEG SHAD	༐
	0x0F11, // TIBETAN MARK RIN CHEN SPUNGS SHAD	༑
	0x0F12, // TIBETAN MARK RGYA GRAM SHAD	༒
	0x0F14, // TIBETAN MARK GTER TSHEG	༔
	0x0F85, // TIBETAN MARK PALUTA	྅
	0x0FD0, // TIBETAN MARK BSKA- SHOG GI MGO RGYAN	࿐
	0x0FD1, // TIBETAN MARK MNYAM YIG GI MGO RGYAN	࿑
	0x0FD2, // TIBETAN MARK NYIS TSHEG	࿒
	0x0FD3, // TIBETAN MARK INITIAL BRDA RNYING YIG MGO MDUN MA	࿓
	0x0FD4, // TIBETAN MARK CLOSING BRDA RNYING YIG MGO SGAB MA	࿔
	0x0FD9, // TIBETAN MARK LEADING MCHAN RTAGS	࿙
	0x0FDA, // TIBETAN MARK TRAILING MCHAN RTAGS	࿚
	0x104A, // MYANMAR SIGN LITTLE SECTION	၊
	0x104B, // MYANMAR SIGN SECTION	။
	0x104C, // MYANMAR SYMBOL LOCATIVE	၌
	0x104D, // MYANMAR SYMBOL COMPLETED	၍
	0x104E, // MYANMAR SYMBOL AFOREMENTIONED	၎
	0x104F, // MYANMAR SYMBOL GENITIVE	၏
	0x10FB, // GEORGIAN PARAGRAPH SEPARATOR	჻
	0x1360, // ETHIOPIC SECTION MARK	፠
	0x1361, // ETHIOPIC WORDSPACE	፡
	0x1362, // ETHIOPIC FULL STOP	።
	0x1363, // ETHIOPIC COMMA	፣
	0x1364, // ETHIOPIC SEMICOLON	፤
	0x1365, // ETHIOPIC COLON	፥
	0x1366, // ETHIOPIC PREFACE COLON	፦
	0x1367, // ETHIOPIC QUESTION MARK	፧
	0x1368, // ETHIOPIC PARAGRAPH SEPARATOR	፨
	0x166E, // CANADIAN SYLLABICS FULL STOP	᙮
	0x16EB, // RUNIC SINGLE PUNCTUATION	᛫
	0x16EC, // RUNIC MULTIPLE PUNCTUATION	᛬
	0x16ED, // RUNIC CROSS PUNCTUATION	᛭
	0x1735, // PHILIPPINE SINGLE PUNCTUATION	᜵
	0x1736, // PHILIPPINE DOUBLE PUNCTUATION	᜶
	0x17D4, // KHMER SIGN KHAN	។
	0x17D5, // KHMER SIGN BARIYOOSAN	៕
	0x17D6, // KHMER SIGN CAMNUC PII KUUH	៖
	0x17D8, // KHMER SIGN BEYYAL	៘
	0x17D9, // KHMER SIGN PHNAEK MUAN	៙
	0x17DA, // KHMER SIGN KOOMUUT	៚
	0x1800, // MONGOLIAN BIRGA	᠀
	0x1801, // MONGOLIAN ELLIPSIS	᠁
	0x1802, // MONGOLIAN COMMA	᠂
	0x1803, // MONGOLIAN FULL STOP	᠃
	0x1804, // MONGOLIAN COLON	᠄
	0x1805, // MONGOLIAN FOUR DOTS	᠅
	0x1807, // MONGOLIAN SIBE SYLLABLE BOUNDARY MARKER	᠇
	0x1808, // MONGOLIAN MANCHU COMMA	᠈
	0x1809, // MONGOLIAN MANCHU FULL STOP	᠉
	0x180A, // MONGOLIAN NIRUGU	᠊
	0x1944, // LIMBU EXCLAMATION MARK	᥄
	0x1945, // LIMBU QUESTION MARK	᥅
	0x1A1E, // BUGINESE PALLAWA	᨞
	0x1A1F, // BUGINESE END OF SECTION	᨟
	0x1AA0, // TAI THAM SIGN WIANG	᪠
	0x1AA1, // TAI THAM SIGN WIANGWAAK	᪡
	0x1AA2, // TAI THAM SIGN SAWAN	᪢
	0x1AA3, // TAI THAM SIGN KEOW	᪣
	0x1AA4, // TAI THAM SIGN HOY	᪤
	0x1AA5, // TAI THAM SIGN DOKMAI	᪥
	0x1AA6, // TAI THAM SIGN REVERSED ROTATED RANA	᪦
	0x1AA8, // TAI THAM SIGN KAAN	᪨
	0x1AA9, // TAI THAM SIGN KAANKUU	᪩
	0x1AAA, // TAI THAM SIGN SATKAAN	᪪
	0x1AAB, // TAI THAM SIGN SATKAANKUU	᪫
	0x1AAC, // TAI THAM SIGN HANG	᪬
	0x1AAD, // TAI THAM SIGN CAANG	᪭
	0x1B5A, // BALINESE PANTI	᭚
	0x1B5B, // BALINESE PAMADA	᭛
	0x1B5C, // BALINESE WINDU	᭜
	0x1B5D, // BALINESE CARIK PAMUNGKAH	᭝
	0x1B5E, // BALINESE CARIK SIKI	᭞
	0x1B5F, // BALINESE CARIK PAREREN	᭟
	0x1B60, // BALINESE PAMENENG	᭠
	0x1BFC, // BATAK SYMBOL BINDU NA METEK	᯼
	0x1BFD, // BATAK SYMBOL BINDU PINARBORAS	᯽
	0x1BFE, // BATAK SYMBOL BINDU JUDUL	᯾
	0x1BFF, // BATAK SYMBOL BINDU PANGOLAT	᯿
	0x1C3B, // LEPCHA PUNCTUATION TA-ROL	᰻
	0x1C3C, // LEPCHA PUNCTUATION NYET THYOOM TA-ROL	᰼
	0x1C3D, // LEPCHA PUNCTUATION CER-WA	᰽
	0x1C3E, // LEPCHA PUNCTUATION TSHOOK CER-WA	᰾
	0x1C3F, // LEPCHA PUNCTUATION TSHOOK	᰿
	0x1C7E, // OL CHIKI PUNCTUATION MUCAAD	᱾
	0x1C7F, // OL CHIKI PUNCTUATION DOUBLE MUCAAD	᱿
	0x1CC0, // SUNDANESE PUNCTUATION BINDU SURYA	᳀
	0x1CC1, // SUNDANESE PUNCTUATION BINDU PANGLONG	᳁
	0x1CC2, // SUNDANESE PUNCTUATION BINDU PURNAMA	᳂
	0x1CC3, // SUNDANESE PUNCTUATION BINDU CAKRA	᳃
	0x1CC4, // SUNDANESE PUNCTUATION BINDU LEU SATANGA	᳄
	0x1CC5, // SUNDANESE PUNCTUATION BINDU KA SATANGA	᳅
	0x1CC6, // SUNDANESE PUNCTUATION BINDU DA SATANGA	᳆
	0x1CC7, // SUNDANESE PUNCTUATION BINDU BA SATANGA	᳇
	0x1CD3, // VEDIC SIGN NIHSHVASA	᳓
	0x2016, // DOUBLE VERTICAL LINE	‖
	0x2017, // DOUBLE LOW LINE	‗
	0x2020, // DAGGER	†
	0x2021, // DOUBLE DAGGER	‡
	0x2022, // BULLET	•
	0x2023, // TRIANGULAR BULLET	‣
	0x2024, // ONE DOT LEADER	․
	0x2025, // TWO DOT LEADER	‥
	0x2026, // HORIZONTAL ELLIPSIS	…
	0x2027, // HYPHENATION POINT	‧
	0x2030, // PER MILLE SIGN	‰
	0x2031, // PER TEN THOUSAND SIGN	‱
	0x2032, // PRIME	′
	0x2033, // DOUBLE PRIME	″
	0x2034, // TRIPLE PRIME	‴
	0x2035, // REVERSED PRIME	‵
	0x2036, // REVERSED DOUBLE PRIME	‶
	0x2037, // REVERSED TRIPLE PRIME	‷
	0x2038, // CARET	‸
	0x203B, // REFERENCE MARK	※
	0x203C, // DOUBLE EXCLAMATION MARK	‼
	0x203D, // INTERROBANG	‽
	0x203E, // OVERLINE	‾
	0x2041, // CARET INSERTION POINT	⁁
	0x2042, // ASTERISM	⁂
	0x2043, // HYPHEN BULLET	⁃
	0x2047, // DOUBLE QUESTION MARK	⁇
	0x2048, // QUESTION EXCLAMATION MARK	⁈
	0x2049, // EXCLAMATION QUESTION MARK	⁉
	0x204A, // TIRONIAN SIGN ET	⁊
	0x204B, // REVERSED PILCROW SIGN	⁋
	0x204C, // BLACK LEFTWARDS BULLET	⁌
	0x204D, // BLACK RIGHTWARDS BULLET	⁍
	0x204E, // LOW ASTERISK	⁎
	0x204F, // REVERSED SEMICOLON	⁏
	0x2050, // CLOSE UP	⁐
	0x2051, // TWO ASTERISKS ALIGNED VERTICALLY	⁑
	0x2053, // SWUNG DASH	⁓
	0x2055, // FLOWER PUNCTUATION MARK	⁕
	0x2056, // THREE DOT PUNCTUATION	⁖
	0x2057, // QUADRUPLE PRIME	⁗
	0x2058, // FOUR DOT PUNCTUATION	⁘
	0x2059, // FIVE DOT PUNCTUATION	⁙
	0x205A, // TWO DOT PUNCTUATION	⁚
	0x205B, // FOUR DOT MARK	⁛
	0x205C, // DOTTED CROSS	⁜
	0x205D, // TRICOLON	⁝
	0x205E, // VERTICAL FOUR DOTS	⁞
	0x2CF9, // COPTIC OLD NUBIAN FULL STOP	⳹
	0x2CFA, // COPTIC OLD NUBIAN DIRECT QUESTION MARK	⳺
	0x2CFB, // COPTIC OLD NUBIAN INDIRECT QUESTION MARK	⳻
	0x2CFC, // COPTIC OLD NUBIAN VERSE DIVIDER	⳼
	0x2CFE, // COPTIC FULL STOP	⳾
	0x2CFF, // COPTIC MORPHOLOGICAL DIVIDER	⳿
	0x2D70, // TIFINAGH SEPARATOR MARK	⵰
	0x2E00, // RIGHT ANGLE SUBSTITUTION MARKER	⸀
	0x2E01, // RIGHT ANGLE DOTTED SUBSTITUTION MARKER	⸁
	0x2E06, // RAISED INTERPOLATION MARKER	⸆
	0x2E07, // RAISED DOTTED INTERPOLATION MARKER	⸇
	0x2E08, // DOTTED TRANSPOSITION MARKER	⸈
	0x2E0B, // RAISED SQUARE	⸋
	0x2E0E, // EDITORIAL CORONIS	⸎
	0x2E0F, // PARAGRAPHOS	⸏
	0x2E10, // FORKED PARAGRAPHOS	⸐
	0x2E11, // REVERSED FORKED PARAGRAPHOS	⸑
	0x2E12, // HYPODIASTOLE	⸒
	0x2E13, // DOTTED OBELOS	⸓
	0x2E14, // DOWNWARDS ANCORA	⸔
	0x2E15, // UPWARDS ANCORA	⸕
	0x2E16, // DOTTED RIGHT-POINTING ANGLE	⸖
	0x2E18, // INVERTED INTERROBANG	⸘
	0x2E19, // PALM BRANCH	⸙
	0x2E1B, // TILDE WITH RING ABOVE	⸛
	0x2E1E, // TILDE WITH DOT ABOVE	⸞
	0x2E1F, // TILDE WITH DOT BELOW	⸟
	0x2E2A, // TWO DOTS OVER ONE DOT PUNCTUATION	⸪
	0x2E2B, // ONE DOT OVER TWO DOTS PUNCTUATION	⸫
	0x2E2C, // SQUARED FOUR DOT PUNCTUATION	⸬
	0x2E2D, // FIVE DOT MARK	⸭
	0x2E2E, // REVERSED QUESTION MARK	⸮
	0x2E30, // RING POINT	⸰
	0x2E31, // WORD SEPARATOR MIDDLE DOT	⸱
	0x2E32, // TURNED COMMA	⸲
	0x2E33, // RAISED DOT	⸳
	0x2E34, // RAISED COMMA	⸴
	0x2E35, // TURNED SEMICOLON	⸵
	0x2E36, // DAGGER WITH LEFT GUARD	⸶
	0x2E37, // DAGGER WITH RIGHT GUARD	⸷
	0x2E38, // TURNED DAGGER	⸸
	0x2E39, // TOP HALF SECTION SIGN	⸹
	0x2E3C, // STENOGRAPHIC FULL STOP	⸼
	0x2E3D, // VERTICAL SIX DOTS	⸽
	0x2E3E, // WIGGLY VERTICAL LINE	⸾
	0x2E3F, // CAPITULUM	⸿
	0x2E41, // REVERSED COMMA	⹁
	0x2E43, // DASH WITH LEFT UPTURN	⹃
	0x2E44, // DOUBLE SUSPENSION MARK	⹄
	0x2E45, // INVERTED LOW KAVYKA	⹅
	0x2E46, // INVERTED LOW KAVYKA WITH KAVYKA ABOVE	⹆
	0x2E47, // LOW KAVYKA	⹇
	0x2E48, // LOW KAVYKA WITH DOT	⹈
	0x2E49, // DOUBLE STACKED COMMA	⹉
	0x2E4A, // DOTTED SOLIDUS	⹊
	0x2E4B, // TRIPLE DAGGER	⹋
	0x2E4C, // MEDIEVAL COMMA	⹌
	0x2E4D, // PARAGRAPHUS MARK	⹍
	0x2E4E, // PUNCTUS ELEVATUS MARK	⹎
	0x2E4F, // CORNISH VERSE DIVIDER	⹏
	0x3001, // IDEOGRAPHIC COMMA	、
	0x3002, // IDEOGRAPHIC FULL STOP	。
	0x3003, // DITTO MARK	〃
	0x303D, // PART ALTERNATION MARK	〽
	0x30FB, // KATAKANA MIDDLE DOT	・
	0xA4FE, // LISU PUNCTUATION COMMA	꓾
	0xA4FF, // LISU PUNCTUATION FULL STOP	꓿
	0xA60D, // VAI COMMA	꘍
	0xA60E, // VAI FULL STOP	꘎
	0xA60F, // VAI QUESTION MARK	꘏
	0xA673, // SLAVONIC ASTERISK	꙳
	0xA67E, // CYRILLIC KAVYKA	꙾
	0xA6F2, // BAMUM NJAEMLI	꛲
	0xA6F3, // BAMUM FULL STOP	꛳
	0xA6F4, // BAMUM COLON	꛴
	0xA6F5, // BAMUM COMMA	꛵
	0xA6F6, // BAMUM SEMICOLON	꛶
	0xA6F7, // BAMUM QUESTION MARK	꛷
	0xA874, // PHAGS-PA SINGLE HEAD MARK	꡴
	0xA875, // PHAGS-PA DOUBLE HEAD MARK	꡵
	0xA876, // PHAGS-PA MARK SHAD	꡶
	0xA877, // PHAGS-PA MARK DOUBLE SHAD	꡷
	0xA8CE, // SAURASHTRA DANDA	꣎
	0xA8CF, // SAURASHTRA DOUBLE DANDA	꣏
	0xA8F8, // DEVANAGARI SIGN PUSHPIKA	꣸
	0xA8F9, // DEVANAGARI GAP FILLER	꣹
	0xA8FA, // DEVANAGARI CARET	꣺
	0xA8FC, // DEVANAGARI SIGN SIDDHAM	꣼
	0xA92E, // KAYAH LI SIGN CWI	꤮
	0xA92F, // KAYAH LI SIGN SHYA	꤯
	0xA95F, // REJANG SECTION MARK	꥟
	0xA9C1, // JAVANESE LEFT RERENGGAN	꧁
	0xA9C2, // JAVANESE RIGHT RERENGGAN	꧂
	0xA9C3, // JAVANESE PADA ANDAP	꧃
	0xA9C4, // JAVANESE PADA MADYA	꧄
	0xA9C5, // JAVANESE PADA LUHUR	꧅
	0xA9C6, // JAVANESE PADA WINDU	꧆
	0xA9C7, // JAVANESE PADA PANGKAT	꧇
	0xA9C8, // JAVANESE PADA LINGSA	꧈
	0xA9C9, // JAVANESE PADA LUNGSI	꧉
	0xA9CA, // JAVANESE PADA ADEG	꧊
	0xA9CB, // JAVANESE PADA ADEG ADEG	꧋
	0xA9CC, // JAVANESE PADA PISELEH	꧌
	0xA9CD, // JAVANESE TURNED PADA PISELEH	꧍
	0xA9DE, // JAVANESE PADA TIRTA TUMETES	꧞
	0xA9DF, // JAVANESE PADA ISEN-ISEN	꧟
	0xAA5C, // CHAM PUNCTUATION SPIRAL	꩜
	0xAA5D, // CHAM PUNCTUATION DANDA	꩝
	0xAA5E, // CHAM PUNCTUATION DOUBLE DANDA	꩞
	0xAA5F, // CHAM PUNCTUATION TRIPLE DANDA	꩟
	0xAADE, // TAI VIET SYMBOL HO HOI	꫞
	0xAADF, // TAI VIET SYMBOL KOI KOI	꫟
	0xAAF0, // MEETEI MAYEK CHEIKHAN	꫰
	0xAAF1, // MEETEI MAYEK AHANG KHUDAM	꫱
	0xABEB, // MEETEI MAYEK CHEIKHEI	꯫
	0xFE10, // PRESENTATION FORM FOR VERTICAL COMMA	︐
	0xFE11, // PRESENTATION FORM FOR VERTICAL IDEOGRAPHIC COMMA	︑
	0xFE12, // PRESENTATION FORM FOR VERTICAL IDEOGRAPHIC FULL STOP	︒
	0xFE13, // PRESENTATION FORM FOR VERTICAL COLON	︓
	0xFE14, // PRESENTATION FORM FOR VERTICAL SEMICOLON	︔
	0xFE15, // PRESENTATION FORM FOR VERTICAL EXCLAMATION MARK	︕
	0xFE16, // PRESENTATION FORM FOR VERTICAL QUESTION MARK	︖
	0xFE19, // PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS	︙
	0xFE30, // PRESENTATION FORM FOR VERTICAL TWO DOT LEADER	︰
	0xFE45, // SESAME DOT	﹅
	0xFE46, // WHITE SESAME DOT	﹆
	0xFE49, // DASHED OVERLINE	﹉
	0xFE4A, // CENTRELINE OVERLINE	﹊
	0xFE4B, // WAVY OVERLINE	﹋
	0xFE4C, // DOUBLE WAVY OVERLINE	﹌
	0xFE50, // SMALL COMMA	﹐
	0xFE51, // SMALL IDEOGRAPHIC COMMA	﹑
	0xFE52, // SMALL FULL STOP	﹒
	0xFE54, // SMALL SEMICOLON	﹔
	0xFE55, // SMALL COLON	﹕
	0xFE56, // SMALL QUESTION MARK	﹖
	0xFE57, // SMALL EXCLAMATION MARK	﹗
	0xFE5F, // SMALL NUMBER SIGN	﹟
	0xFE60, // SMALL AMPERSAND	﹠
	0xFE61, // SMALL ASTERISK	﹡
	0xFE68, // SMALL REVERSE SOLIDUS	﹨
	0xFE6A, // SMALL PERCENT SIGN	﹪
	0xFE6B, // SMALL COMMERCIAL AT	﹫
	0xFF01, // FULLWIDTH EXCLAMATION MARK	！
	0xFF02, // FULLWIDTH QUOTATION MARK	＂
	0xFF03, // FULLWIDTH NUMBER SIGN	＃
	0xFF05, // FULLWIDTH PERCENT SIGN	％
	0xFF06, // FULLWIDTH AMPERSAND	＆
	0xFF07, // FULLWIDTH APOSTROPHE	＇
	0xFF0A, // FULLWIDTH ASTERISK	＊
	0xFF0C, // FULLWIDTH COMMA	，
	0xFF0E, // FULLWIDTH FULL STOP	．
	0xFF0F, // FULLWIDTH SOLIDUS	／
	0xFF1A, // FULLWIDTH COLON	：
	0xFF1B, // FULLWIDTH SEMICOLON	；
	0xFF1F, // FULLWIDTH QUESTION MARK	？
	0xFF20, // FULLWIDTH COMMERCIAL AT	＠
	0xFF3C, // FULLWIDTH REVERSE SOLIDUS	＼
	0xFF61, // HALFWIDTH IDEOGRAPHIC FULL STOP	｡
	0xFF64, // HALFWIDTH IDEOGRAPHIC COMMA	､
	0xFF65, // HALFWIDTH KATAKANA MIDDLE DOT	･
	0x10100, // AEGEAN WORD SEPARATOR LINE	𐄀
	0x10101, // AEGEAN WORD SEPARATOR DOT	𐄁
	0x10102, // AEGEAN CHECK MARK	𐄂
	0x1039F, // UGARITIC WORD DIVIDER	𐎟
	0x103D0, // OLD PERSIAN WORD DIVIDER	𐏐
	0x1056F, // CAUCASIAN ALBANIAN CITATION MARK	𐕯
	0x10857, // IMPERIAL ARAMAIC SECTION SIGN	𐡗
	0x1091F, // PHOENICIAN WORD SEPARATOR	𐤟
	0x1093F, // LYDIAN TRIANGULAR MARK	𐤿
	0x10A50, // KHAROSHTHI PUNCTUATION DOT	𐩐
	0x10A51, // KHAROSHTHI PUNCTUATION SMALL CIRCLE	𐩑
	0x10A52, // KHAROSHTHI PUNCTUATION CIRCLE	𐩒
	0x10A53, // KHAROSHTHI PUNCTUATION CRESCENT BAR	𐩓
	0x10A54, // KHAROSHTHI PUNCTUATION MANGALAM	𐩔
	0x10A55, // KHAROSHTHI PUNCTUATION LOTUS	𐩕
	0x10A56, // KHAROSHTHI PUNCTUATION DANDA	𐩖
	0x10A57, // KHAROSHTHI PUNCTUATION DOUBLE DANDA	𐩗
	0x10A58, // KHAROSHTHI PUNCTUATION LINES	𐩘
	0x10A7F, // OLD SOUTH ARABIAN NUMERIC INDICATOR	𐩿
	0x10AF0, // MANICHAEAN PUNCTUATION STAR	𐫰
	0x10AF1, // MANICHAEAN PUNCTUATION FLEURON	𐫱
	0x10AF2, // MANICHAEAN PUNCTUATION DOUBLE DOT WITHIN DOT	𐫲
	0x10AF3, // MANICHAEAN PUNCTUATION DOT WITHIN DOT	𐫳
	0x10AF4, // MANICHAEAN PUNCTUATION DOT	𐫴
	0x10AF5, // MANICHAEAN PUNCTUATION TWO DOTS	𐫵
	0x10AF6, // MANICHAEAN PUNCTUATION LINE FILLER	𐫶
	0x10B39, // AVESTAN ABBREVIATION MARK	𐬹
	0x10B3A, // TINY TWO DOTS OVER ONE DOT PUNCTUATION	𐬺
	0x10B3B, // SMALL TWO DOTS OVER ONE DOT PUNCTUATION	𐬻
	0x10B3C, // LARGE TWO DOTS OVER ONE DOT PUNCTUATION	𐬼
	0x10B3D, // LARGE ONE DOT OVER TWO DOTS PUNCTUATION	𐬽
	0x10B3E, // LARGE TWO RINGS OVER ONE RING PUNCTUATION	𐬾
	0x10B3F, // LARGE ONE RING OVER TWO RINGS PUNCTUATION	𐬿
	0x10B99, // PSALTER PAHLAVI SECTION MARK	𐮙
	0x10B9A, // PSALTER PAHLAVI TURNED SECTION MARK	𐮚
	0x10B9B, // PSALTER PAHLAVI FOUR DOTS WITH CROSS	𐮛
	0x10B9C, // PSALTER PAHLAVI FOUR DOTS WITH DOT	𐮜
	0x10F55, // SOGDIAN PUNCTUATION TWO VERTICAL BARS	𐽕
	0x10F56, // SOGDIAN PUNCTUATION TWO VERTICAL BARS WITH DOTS	𐽖
	0x10F57, // SOGDIAN PUNCTUATION CIRCLE WITH DOT	𐽗
	0x10F58, // SOGDIAN PUNCTUATION TWO CIRCLES WITH DOTS	𐽘
	0x10F59, // SOGDIAN PUNCTUATION HALF CIRCLE WITH DOT	𐽙
	0x11047, // BRAHMI DANDA	𑁇
	0x11048, // BRAHMI DOUBLE DANDA	𑁈
	0x11049, // BRAHMI PUNCTUATION DOT	𑁉
	0x1104A, // BRAHMI PUNCTUATION DOUBLE DOT	𑁊
	0x1104B, // BRAHMI PUNCTUATION LINE	𑁋
	0x1104C, // BRAHMI PUNCTUATION CRESCENT BAR	𑁌
	0x1104D, // BRAHMI PUNCTUATION LOTUS	𑁍
	0x110BB, // KAITHI ABBREVIATION SIGN	𑂻
	0x110BC, // KAITHI ENUMERATION SIGN	𑂼
	0x110BE, // KAITHI SECTION MARK	𑂾
	0x110BF, // KAITHI DOUBLE SECTION MARK	𑂿
	0x110C0, // KAITHI DANDA	𑃀
	0x110C1, // KAITHI DOUBLE DANDA	𑃁
	0x11140, // CHAKMA SECTION MARK	𑅀
	0x11141, // CHAKMA DANDA	𑅁
	0x11142, // CHAKMA DOUBLE DANDA	𑅂
	0x11143, // CHAKMA QUESTION MARK	𑅃
	0x11174, // MAHAJANI ABBREVIATION SIGN	𑅴
	0x11175, // MAHAJANI SECTION MARK	𑅵
	0x111C5, // SHARADA DANDA	𑇅
	0x111C6, // SHARADA DOUBLE DANDA	𑇆
	0x111C7, // SHARADA ABBREVIATION SIGN	𑇇
	0x111C8, // SHARADA SEPARATOR	𑇈
	0x111CD, // SHARADA SUTRA MARK	𑇍
	0x111DB, // SHARADA SIGN SIDDHAM	𑇛
	0x111DD, // SHARADA CONTINUATION SIGN	𑇝
	0x111DE, // SHARADA SECTION MARK-1	𑇞
	0x111DF, // SHARADA SECTION MARK-2	𑇟
	0x11238, // KHOJKI DANDA	𑈸
	0x11239, // KHOJKI DOUBLE DANDA	𑈹
	0x1123A, // KHOJKI WORD SEPARATOR	𑈺
	0x1123B, // KHOJKI SECTION MARK	𑈻
	0x1123C, // KHOJKI DOUBLE SECTION MARK	𑈼
	0x1123D, // KHOJKI ABBREVIATION SIGN	𑈽
	0x112A9, // MULTANI SECTION MARK	𑊩
	0x1144B, // NEWA DANDA	𑑋
	0x1144C, // NEWA DOUBLE DANDA	𑑌
	0x1144D, // NEWA COMMA	𑑍
	0x1144E, // NEWA GAP FILLER	𑑎
	0x1144F, // NEWA ABBREVIATION SIGN	𑑏
	0x1145B, // NEWA PLACEHOLDER MARK	𑑛
	0x1145D, // NEWA INSERTION SIGN	𑑝
	0x114C6, // TIRHUTA ABBREVIATION SIGN	𑓆
	0x115C1, // SIDDHAM SIGN SIDDHAM	𑗁
	0x115C2, // SIDDHAM DANDA	𑗂
	0x115C3, // SIDDHAM DOUBLE DANDA	𑗃
	0x115C4, // SIDDHAM SEPARATOR DOT	𑗄
	0x115C5, // SIDDHAM SEPARATOR BAR	𑗅
	0x115C6, // SIDDHAM REPETITION MARK-1	𑗆
	0x115C7, // SIDDHAM REPETITION MARK-2	𑗇
	0x115C8, // SIDDHAM REPETITION MARK-3	𑗈
	0x115C9, // SIDDHAM END OF TEXT MARK	𑗉
	0x115CA, // SIDDHAM SECTION MARK WITH TRIDENT AND U-SHAPED ORNAMENTS	𑗊
	0x115CB, // SIDDHAM SECTION MARK WITH TRIDENT AND DOTTED CRESCENTS	𑗋
	0x115CC, // SIDDHAM SECTION MARK WITH RAYS AND DOTTED CRESCENTS	𑗌
	0x115CD, // SIDDHAM SECTION MARK WITH RAYS AND DOTTED DOUBLE CRESCENTS	𑗍
	0x115CE, // SIDDHAM SECTION MARK WITH RAYS AND DOTTED TRIPLE CRESCENTS	𑗎
	0x115CF, // SIDDHAM SECTION MARK DOUBLE RING	𑗏
	0x115D0, // SIDDHAM SECTION MARK DOUBLE RING WITH RAYS	𑗐
	0x115D1, // SIDDHAM SECTION MARK WITH DOUBLE CRESCENTS	𑗑
	0x115D2, // SIDDHAM SECTION MARK WITH TRIPLE CRESCENTS	𑗒
	0x115D3, // SIDDHAM SECTION MARK WITH QUADRUPLE CRESCENTS	𑗓
	0x115D4, // SIDDHAM SECTION MARK WITH SEPTUPLE CRESCENTS	𑗔
	0x115D5, // SIDDHAM SECTION MARK WITH CIRCLES AND RAYS	𑗕
	0x115D6, // SIDDHAM SECTION MARK WITH CIRCLES AND TWO ENCLOSURES	𑗖
	0x115D7, // SIDDHAM SECTION MARK WITH CIRCLES AND FOUR ENCLOSURES	𑗗
	0x11641, // MODI DANDA	𑙁
	0x11642, // MODI DOUBLE DANDA	𑙂
	0x11643, // MODI ABBREVIATION SIGN	𑙃
	0x11660, // MONGOLIAN BIRGA WITH ORNAMENT	𑙠
	0x11661, // MONGOLIAN ROTATED BIRGA	𑙡
	0x11662, // MONGOLIAN DOUBLE BIRGA WITH ORNAMENT	𑙢
	0x11663, // MONGOLIAN TRIPLE BIRGA WITH ORNAMENT	𑙣
	0x11664, // MONGOLIAN BIRGA WITH DOUBLE ORNAMENT	𑙤
	0x11665, // MONGOLIAN ROTATED BIRGA WITH ORNAMENT	𑙥
	0x11666, // MONGOLIAN ROTATED BIRGA WITH DOUBLE ORNAMENT	𑙦
	0x11667, // MONGOLIAN INVERTED BIRGA	𑙧
	0x11668, // MONGOLIAN INVERTED BIRGA WITH DOUBLE ORNAMENT	𑙨
	0x11669, // MONGOLIAN SWIRL BIRGA	𑙩
	0x1166A, // MONGOLIAN SWIRL BIRGA WITH ORNAMENT	𑙪
	0x1166B, // MONGOLIAN SWIRL BIRGA WITH DOUBLE ORNAMENT	𑙫
	0x1166C, // MONGOLIAN TURNED SWIRL BIRGA WITH DOUBLE ORNAMENT	𑙬
	0x1173C, // AHOM SIGN SMALL SECTION	𑜼
	0x1173D, // AHOM SIGN SECTION	𑜽
	0x1173E, // AHOM SIGN RULAI	𑜾
	0x1183B, // DOGRA ABBREVIATION SIGN	𑠻
	0x119E2, // NANDINAGARI SIGN SIDDHAM	𑧢
	0x11A3F, // ZANABAZAR SQUARE INITIAL HEAD MARK	𑨿
	0x11A40, // ZANABAZAR SQUARE CLOSING HEAD MARK	𑩀
	0x11A41, // ZANABAZAR SQUARE MARK TSHEG	𑩁
	0x11A42, // ZANABAZAR SQUARE MARK SHAD	𑩂
	0x11A43, // ZANABAZAR SQUARE MARK DOUBLE SHAD	𑩃
	0x11A44, // ZANABAZAR SQUARE MARK LONG TSHEG	𑩄
	0x11A45, // ZANABAZAR SQUARE INITIAL DOUBLE-LINED HEAD MARK	𑩅
	0x11A46, // ZANABAZAR SQUARE CLOSING DOUBLE-LINED HEAD MARK	𑩆
	0x11A9A, // SOYOMBO MARK TSHEG	𑪚
	0x11A9B, // SOYOMBO MARK SHAD	𑪛
	0x11A9C, // SOYOMBO MARK DOUBLE SHAD	𑪜
	0x11A9E, // SOYOMBO HEAD MARK WITH MOON AND SUN AND TRIPLE FLAME	𑪞
	0x11A9F, // SOYOMBO HEAD MARK WITH MOON AND SUN AND FLAME	𑪟
	0x11AA0, // SOYOMBO HEAD MARK WITH MOON AND SUN	𑪠
	0x11AA1, // SOYOMBO TERMINAL MARK-1	𑪡
	0x11AA2, // SOYOMBO TERMINAL MARK-2	𑪢
	0x11C41, // BHAIKSUKI DANDA	𑱁
	0x11C42, // BHAIKSUKI DOUBLE DANDA	𑱂
	0x11C43, // BHAIKSUKI WORD SEPARATOR	𑱃
	0x11C44, // BHAIKSUKI GAP FILLER-1	𑱄
	0x11C45, // BHAIKSUKI GAP FILLER-2	𑱅
	0x11C70, // MARCHEN HEAD MARK	𑱰
	0x11C71, // MARCHEN MARK SHAD	𑱱
	0x11EF7, // MAKASAR PASSIMBANG	𑻷
	0x11EF8, // MAKASAR END OF SECTION	𑻸
	0x11FFF, // TAMIL PUNCTUATION END OF TEXT	𑿿
	0x12470, // CUNEIFORM PUNCTUATION SIGN OLD ASSYRIAN WORD DIVIDER	𒑰
	0x12471, // CUNEIFORM PUNCTUATION SIGN VERTICAL COLON	𒑱
	0x12472, // CUNEIFORM PUNCTUATION SIGN DIAGONAL COLON	𒑲
	0x12473, // CUNEIFORM PUNCTUATION SIGN DIAGONAL TRICOLON	𒑳
	0x12474, // CUNEIFORM PUNCTUATION SIGN DIAGONAL QUADCOLON	𒑴
	0x16A6E, // MRO DANDA	𖩮
	0x16A6F, // MRO DOUBLE DANDA	𖩯
	0x16AF5, // BASSA VAH FULL STOP	𖫵
	0x16B37, // PAHAWH HMONG SIGN VOS THOM	𖬷
	0x16B38, // PAHAWH HMONG SIGN VOS TSHAB CEEB	𖬸
	0x16B39, // PAHAWH HMONG SIGN CIM CHEEM	𖬹
	0x16B3A, // PAHAWH HMONG SIGN VOS THIAB	𖬺
	0x16B3B, // PAHAWH HMONG SIGN VOS FEEM	𖬻
	0x16B44, // PAHAWH HMONG SIGN XAUS	𖭄
	0x16E97, // MEDEFAIDRIN COMMA	𖺗
	0x16E98, // MEDEFAIDRIN FULL STOP	𖺘
	0x16E99, // MEDEFAIDRIN SYMBOL AIVA	𖺙
	0x16E9A, // MEDEFAIDRIN EXCLAMATION OH	𖺚
	0x16FE2, // OLD CHINESE HOOK MARK	𖿢
	0x1BC9F, // DUPLOYAN PUNCTUATION CHINOOK FULL STOP	𛲟
	0x1DA87, // SIGNWRITING COMMA	𝪇
	0x1DA88, // SIGNWRITING FULL STOP	𝪈
	0x1DA89, // SIGNWRITING SEMICOLON	𝪉
	0x1DA8A, // SIGNWRITING COLON	𝪊
	0x1DA8B, // SIGNWRITING PARENTHESIS	𝪋
	0x1E95E, // ADLAM INITIAL EXCLAMATION MARK	𞥞
	0x1E95F, // ADLAM INITIAL QUESTION MARK
]
