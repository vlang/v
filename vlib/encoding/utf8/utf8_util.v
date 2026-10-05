// utf-8 utility string functions
//
// Copyright (c) 2019-2024 Dario Deledda. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module utf8

// Utility functions

const replacement_rune = rune(0xfffd)

@[inline]
fn is_continuation(b u8) bool {
	return (b & 0xc0) == 0x80
}

@[direct_array_access]
fn decode_rune_at(s string, index int) (rune, int) {
	if s.len == 0 || index < 0 || index >= s.len {
		return 0, 0
	}
	b0 := s[index]
	if b0 < 0x80 {
		return rune(b0), 1
	}
	if b0 < 0xc2 {
		return replacement_rune, 1
	}
	ch_len := if b0 < 0xe0 {
		2
	} else if b0 < 0xf0 {
		3
	} else if b0 < 0xf5 {
		4
	} else {
		return replacement_rune, 1
	}
	if index + ch_len > s.len {
		return replacement_rune, 1
	}
	b1 := s[index + 1]
	if !is_continuation(b1) {
		return replacement_rune, 1
	}
	if ch_len == 2 {
		return ((rune(b0) & 0x1f) << 6) | (rune(b1) & 0x3f), 2
	}
	if b0 == 0xe0 && b1 < 0xa0 {
		return replacement_rune, 1
	}
	if b0 == 0xed && b1 >= 0xa0 {
		return replacement_rune, 1
	}
	b2 := s[index + 2]
	if !is_continuation(b2) {
		return replacement_rune, 1
	}
	if ch_len == 3 {
		return ((rune(b0) & 0x0f) << 12) | ((rune(b1) & 0x3f) << 6) | (rune(b2) & 0x3f), 3
	}
	if b0 == 0xf0 && b1 < 0x90 {
		return replacement_rune, 1
	}
	if b0 == 0xf4 && b1 > 0x8f {
		return replacement_rune, 1
	}
	b3 := s[index + 3]
	if !is_continuation(b3) {
		return replacement_rune, 1
	}
	return ((rune(b0) & 0x07) << 18) | ((rune(b1) & 0x3f) << 12) | ((rune(b2) & 0x3f) << 6) | (rune(b3) & 0x3f), 4
}

// len return the length as number of unicode chars from a string
pub fn len(s string) int {
	mut count := 0
	mut index := 0

	for index < s.len {
		_, ch_len := decode_rune_at(s, index)
		if ch_len == 0 {
			break
		}
		index += ch_len
		count++
	}
	return count
}

// get_rune convert a UTF-8 unicode codepoint in string[index] into a UTF-32 encoded rune
pub fn get_rune(s string, index int) rune {
	r, _ := decode_rune_at(s, index)
	return r
}

// raw_index - get the raw unicode character from the UTF-8 string by the given index value as UTF-8 string.
// example: utf8.raw_index('我是V Lang', 1) => '是'
pub fn raw_index(s string, index int) string {
	mut r := []rune{}

	mut i := 0
	for i < s.len {
		decoded, ch_len := decode_rune_at(s, i)
		if ch_len == 0 {
			break
		}
		r << decoded
		if r.len - 1 == index {
			break
		}
		i += ch_len
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

// is_punct returns true if the rune starting at the byte offset index belongs to
// a Unicode punctuation category. It recognizes punctuation from every script,
// with the same Unicode 15.0.0 membership as is_global_punct.
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

// is_rune_punct return true if the input unicode is a unicode punctuation
pub fn is_rune_punct(r rune) bool {
	return find_punct_in_table(r, unicode_punct) != rune(-1)
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
		_, ch_len := decode_rune_at(s, index)

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
	rune(0x0021),
	rune(0x0022),
	rune(0x0023),
	rune(0x0025),
	rune(0x0026),
	rune(0x0027),
	rune(0x0028),
	rune(0x0029),
	rune(0x002A),
	rune(0x002C),
	rune(0x002D),
	rune(0x002E),
	rune(0x002F),
	rune(0x003A),
	rune(0x003B),
	rune(0x003F),
	rune(0x0040),
	rune(0x005B),
	rune(0x005C),
	rune(0x005D),
	rune(0x005F),
	rune(0x007B),
	rune(0x007D),
	rune(0x00A1),
	rune(0x00A7),
	rune(0x00AB),
	rune(0x00B6),
	rune(0x00B7),
	rune(0x00BB),
	rune(0x00BF),
	rune(0x037E),
	rune(0x0387),
	rune(0x055A),
	rune(0x055B),
	rune(0x055C),
	rune(0x055D),
	rune(0x055E),
	rune(0x055F),
	rune(0x0589),
	rune(0x058A),
	rune(0x05BE),
	rune(0x05C0),
	rune(0x05C3),
	rune(0x05C6),
	rune(0x05F3),
	rune(0x05F4),
	rune(0x0609),
	rune(0x060A),
	rune(0x060C),
	rune(0x060D),
	rune(0x061B),
	rune(0x061D),
	rune(0x061E),
	rune(0x061F),
	rune(0x066A),
	rune(0x066B),
	rune(0x066C),
	rune(0x066D),
	rune(0x06D4),
	rune(0x0700),
	rune(0x0701),
	rune(0x0702),
	rune(0x0703),
	rune(0x0704),
	rune(0x0705),
	rune(0x0706),
	rune(0x0707),
	rune(0x0708),
	rune(0x0709),
	rune(0x070A),
	rune(0x070B),
	rune(0x070C),
	rune(0x070D),
	rune(0x07F7),
	rune(0x07F8),
	rune(0x07F9),
	rune(0x0830),
	rune(0x0831),
	rune(0x0832),
	rune(0x0833),
	rune(0x0834),
	rune(0x0835),
	rune(0x0836),
	rune(0x0837),
	rune(0x0838),
	rune(0x0839),
	rune(0x083A),
	rune(0x083B),
	rune(0x083C),
	rune(0x083D),
	rune(0x083E),
	rune(0x085E),
	rune(0x0964),
	rune(0x0965),
	rune(0x0970),
	rune(0x09FD),
	rune(0x0A76),
	rune(0x0AF0),
	rune(0x0C77),
	rune(0x0C84),
	rune(0x0DF4),
	rune(0x0E4F),
	rune(0x0E5A),
	rune(0x0E5B),
	rune(0x0F04),
	rune(0x0F05),
	rune(0x0F06),
	rune(0x0F07),
	rune(0x0F08),
	rune(0x0F09),
	rune(0x0F0A),
	rune(0x0F0B),
	rune(0x0F0C),
	rune(0x0F0D),
	rune(0x0F0E),
	rune(0x0F0F),
	rune(0x0F10),
	rune(0x0F11),
	rune(0x0F12),
	rune(0x0F14),
	rune(0x0F3A),
	rune(0x0F3B),
	rune(0x0F3C),
	rune(0x0F3D),
	rune(0x0F85),
	rune(0x0FD0),
	rune(0x0FD1),
	rune(0x0FD2),
	rune(0x0FD3),
	rune(0x0FD4),
	rune(0x0FD9),
	rune(0x0FDA),
	rune(0x104A),
	rune(0x104B),
	rune(0x104C),
	rune(0x104D),
	rune(0x104E),
	rune(0x104F),
	rune(0x10FB),
	rune(0x1360),
	rune(0x1361),
	rune(0x1362),
	rune(0x1363),
	rune(0x1364),
	rune(0x1365),
	rune(0x1366),
	rune(0x1367),
	rune(0x1368),
	rune(0x1400),
	rune(0x166E),
	rune(0x169B),
	rune(0x169C),
	rune(0x16EB),
	rune(0x16EC),
	rune(0x16ED),
	rune(0x1735),
	rune(0x1736),
	rune(0x17D4),
	rune(0x17D5),
	rune(0x17D6),
	rune(0x17D8),
	rune(0x17D9),
	rune(0x17DA),
	rune(0x1800),
	rune(0x1801),
	rune(0x1802),
	rune(0x1803),
	rune(0x1804),
	rune(0x1805),
	rune(0x1806),
	rune(0x1807),
	rune(0x1808),
	rune(0x1809),
	rune(0x180A),
	rune(0x1944),
	rune(0x1945),
	rune(0x1A1E),
	rune(0x1A1F),
	rune(0x1AA0),
	rune(0x1AA1),
	rune(0x1AA2),
	rune(0x1AA3),
	rune(0x1AA4),
	rune(0x1AA5),
	rune(0x1AA6),
	rune(0x1AA8),
	rune(0x1AA9),
	rune(0x1AAA),
	rune(0x1AAB),
	rune(0x1AAC),
	rune(0x1AAD),
	rune(0x1B5A),
	rune(0x1B5B),
	rune(0x1B5C),
	rune(0x1B5D),
	rune(0x1B5E),
	rune(0x1B5F),
	rune(0x1B60),
	rune(0x1B7D),
	rune(0x1B7E),
	rune(0x1BFC),
	rune(0x1BFD),
	rune(0x1BFE),
	rune(0x1BFF),
	rune(0x1C3B),
	rune(0x1C3C),
	rune(0x1C3D),
	rune(0x1C3E),
	rune(0x1C3F),
	rune(0x1C7E),
	rune(0x1C7F),
	rune(0x1CC0),
	rune(0x1CC1),
	rune(0x1CC2),
	rune(0x1CC3),
	rune(0x1CC4),
	rune(0x1CC5),
	rune(0x1CC6),
	rune(0x1CC7),
	rune(0x1CD3),
	rune(0x2010),
	rune(0x2011),
	rune(0x2012),
	rune(0x2013),
	rune(0x2014),
	rune(0x2015),
	rune(0x2016),
	rune(0x2017),
	rune(0x2018),
	rune(0x2019),
	rune(0x201A),
	rune(0x201B),
	rune(0x201C),
	rune(0x201D),
	rune(0x201E),
	rune(0x201F),
	rune(0x2020),
	rune(0x2021),
	rune(0x2022),
	rune(0x2023),
	rune(0x2024),
	rune(0x2025),
	rune(0x2026),
	rune(0x2027),
	rune(0x2030),
	rune(0x2031),
	rune(0x2032),
	rune(0x2033),
	rune(0x2034),
	rune(0x2035),
	rune(0x2036),
	rune(0x2037),
	rune(0x2038),
	rune(0x2039),
	rune(0x203A),
	rune(0x203B),
	rune(0x203C),
	rune(0x203D),
	rune(0x203E),
	rune(0x203F),
	rune(0x2040),
	rune(0x2041),
	rune(0x2042),
	rune(0x2043),
	rune(0x2045),
	rune(0x2046),
	rune(0x2047),
	rune(0x2048),
	rune(0x2049),
	rune(0x204A),
	rune(0x204B),
	rune(0x204C),
	rune(0x204D),
	rune(0x204E),
	rune(0x204F),
	rune(0x2050),
	rune(0x2051),
	rune(0x2053),
	rune(0x2054),
	rune(0x2055),
	rune(0x2056),
	rune(0x2057),
	rune(0x2058),
	rune(0x2059),
	rune(0x205A),
	rune(0x205B),
	rune(0x205C),
	rune(0x205D),
	rune(0x205E),
	rune(0x207D),
	rune(0x207E),
	rune(0x208D),
	rune(0x208E),
	rune(0x2308),
	rune(0x2309),
	rune(0x230A),
	rune(0x230B),
	rune(0x2329),
	rune(0x232A),
	rune(0x2768),
	rune(0x2769),
	rune(0x276A),
	rune(0x276B),
	rune(0x276C),
	rune(0x276D),
	rune(0x276E),
	rune(0x276F),
	rune(0x2770),
	rune(0x2771),
	rune(0x2772),
	rune(0x2773),
	rune(0x2774),
	rune(0x2775),
	rune(0x27C5),
	rune(0x27C6),
	rune(0x27E6),
	rune(0x27E7),
	rune(0x27E8),
	rune(0x27E9),
	rune(0x27EA),
	rune(0x27EB),
	rune(0x27EC),
	rune(0x27ED),
	rune(0x27EE),
	rune(0x27EF),
	rune(0x2983),
	rune(0x2984),
	rune(0x2985),
	rune(0x2986),
	rune(0x2987),
	rune(0x2988),
	rune(0x2989),
	rune(0x298A),
	rune(0x298B),
	rune(0x298C),
	rune(0x298D),
	rune(0x298E),
	rune(0x298F),
	rune(0x2990),
	rune(0x2991),
	rune(0x2992),
	rune(0x2993),
	rune(0x2994),
	rune(0x2995),
	rune(0x2996),
	rune(0x2997),
	rune(0x2998),
	rune(0x29D8),
	rune(0x29D9),
	rune(0x29DA),
	rune(0x29DB),
	rune(0x29FC),
	rune(0x29FD),
	rune(0x2CF9),
	rune(0x2CFA),
	rune(0x2CFB),
	rune(0x2CFC),
	rune(0x2CFE),
	rune(0x2CFF),
	rune(0x2D70),
	rune(0x2E00),
	rune(0x2E01),
	rune(0x2E02),
	rune(0x2E03),
	rune(0x2E04),
	rune(0x2E05),
	rune(0x2E06),
	rune(0x2E07),
	rune(0x2E08),
	rune(0x2E09),
	rune(0x2E0A),
	rune(0x2E0B),
	rune(0x2E0C),
	rune(0x2E0D),
	rune(0x2E0E),
	rune(0x2E0F),
	rune(0x2E10),
	rune(0x2E11),
	rune(0x2E12),
	rune(0x2E13),
	rune(0x2E14),
	rune(0x2E15),
	rune(0x2E16),
	rune(0x2E17),
	rune(0x2E18),
	rune(0x2E19),
	rune(0x2E1A),
	rune(0x2E1B),
	rune(0x2E1C),
	rune(0x2E1D),
	rune(0x2E1E),
	rune(0x2E1F),
	rune(0x2E20),
	rune(0x2E21),
	rune(0x2E22),
	rune(0x2E23),
	rune(0x2E24),
	rune(0x2E25),
	rune(0x2E26),
	rune(0x2E27),
	rune(0x2E28),
	rune(0x2E29),
	rune(0x2E2A),
	rune(0x2E2B),
	rune(0x2E2C),
	rune(0x2E2D),
	rune(0x2E2E),
	rune(0x2E30),
	rune(0x2E31),
	rune(0x2E32),
	rune(0x2E33),
	rune(0x2E34),
	rune(0x2E35),
	rune(0x2E36),
	rune(0x2E37),
	rune(0x2E38),
	rune(0x2E39),
	rune(0x2E3A),
	rune(0x2E3B),
	rune(0x2E3C),
	rune(0x2E3D),
	rune(0x2E3E),
	rune(0x2E3F),
	rune(0x2E40),
	rune(0x2E41),
	rune(0x2E42),
	rune(0x2E43),
	rune(0x2E44),
	rune(0x2E45),
	rune(0x2E46),
	rune(0x2E47),
	rune(0x2E48),
	rune(0x2E49),
	rune(0x2E4A),
	rune(0x2E4B),
	rune(0x2E4C),
	rune(0x2E4D),
	rune(0x2E4E),
	rune(0x2E4F),
	rune(0x2E52),
	rune(0x2E53),
	rune(0x2E54),
	rune(0x2E55),
	rune(0x2E56),
	rune(0x2E57),
	rune(0x2E58),
	rune(0x2E59),
	rune(0x2E5A),
	rune(0x2E5B),
	rune(0x2E5C),
	rune(0x2E5D),
	rune(0x3001),
	rune(0x3002),
	rune(0x3003),
	rune(0x3008),
	rune(0x3009),
	rune(0x300A),
	rune(0x300B),
	rune(0x300C),
	rune(0x300D),
	rune(0x300E),
	rune(0x300F),
	rune(0x3010),
	rune(0x3011),
	rune(0x3014),
	rune(0x3015),
	rune(0x3016),
	rune(0x3017),
	rune(0x3018),
	rune(0x3019),
	rune(0x301A),
	rune(0x301B),
	rune(0x301C),
	rune(0x301D),
	rune(0x301E),
	rune(0x301F),
	rune(0x3030),
	rune(0x303D),
	rune(0x30A0),
	rune(0x30FB),
	rune(0xA4FE),
	rune(0xA4FF),
	rune(0xA60D),
	rune(0xA60E),
	rune(0xA60F),
	rune(0xA673),
	rune(0xA67E),
	rune(0xA6F2),
	rune(0xA6F3),
	rune(0xA6F4),
	rune(0xA6F5),
	rune(0xA6F6),
	rune(0xA6F7),
	rune(0xA874),
	rune(0xA875),
	rune(0xA876),
	rune(0xA877),
	rune(0xA8CE),
	rune(0xA8CF),
	rune(0xA8F8),
	rune(0xA8F9),
	rune(0xA8FA),
	rune(0xA8FC),
	rune(0xA92E),
	rune(0xA92F),
	rune(0xA95F),
	rune(0xA9C1),
	rune(0xA9C2),
	rune(0xA9C3),
	rune(0xA9C4),
	rune(0xA9C5),
	rune(0xA9C6),
	rune(0xA9C7),
	rune(0xA9C8),
	rune(0xA9C9),
	rune(0xA9CA),
	rune(0xA9CB),
	rune(0xA9CC),
	rune(0xA9CD),
	rune(0xA9DE),
	rune(0xA9DF),
	rune(0xAA5C),
	rune(0xAA5D),
	rune(0xAA5E),
	rune(0xAA5F),
	rune(0xAADE),
	rune(0xAADF),
	rune(0xAAF0),
	rune(0xAAF1),
	rune(0xABEB),
	rune(0xFD3E),
	rune(0xFD3F),
	rune(0xFE10),
	rune(0xFE11),
	rune(0xFE12),
	rune(0xFE13),
	rune(0xFE14),
	rune(0xFE15),
	rune(0xFE16),
	rune(0xFE17),
	rune(0xFE18),
	rune(0xFE19),
	rune(0xFE30),
	rune(0xFE31),
	rune(0xFE32),
	rune(0xFE33),
	rune(0xFE34),
	rune(0xFE35),
	rune(0xFE36),
	rune(0xFE37),
	rune(0xFE38),
	rune(0xFE39),
	rune(0xFE3A),
	rune(0xFE3B),
	rune(0xFE3C),
	rune(0xFE3D),
	rune(0xFE3E),
	rune(0xFE3F),
	rune(0xFE40),
	rune(0xFE41),
	rune(0xFE42),
	rune(0xFE43),
	rune(0xFE44),
	rune(0xFE45),
	rune(0xFE46),
	rune(0xFE47),
	rune(0xFE48),
	rune(0xFE49),
	rune(0xFE4A),
	rune(0xFE4B),
	rune(0xFE4C),
	rune(0xFE4D),
	rune(0xFE4E),
	rune(0xFE4F),
	rune(0xFE50),
	rune(0xFE51),
	rune(0xFE52),
	rune(0xFE54),
	rune(0xFE55),
	rune(0xFE56),
	rune(0xFE57),
	rune(0xFE58),
	rune(0xFE59),
	rune(0xFE5A),
	rune(0xFE5B),
	rune(0xFE5C),
	rune(0xFE5D),
	rune(0xFE5E),
	rune(0xFE5F),
	rune(0xFE60),
	rune(0xFE61),
	rune(0xFE63),
	rune(0xFE68),
	rune(0xFE6A),
	rune(0xFE6B),
	rune(0xFF01),
	rune(0xFF02),
	rune(0xFF03),
	rune(0xFF05),
	rune(0xFF06),
	rune(0xFF07),
	rune(0xFF08),
	rune(0xFF09),
	rune(0xFF0A),
	rune(0xFF0C),
	rune(0xFF0D),
	rune(0xFF0E),
	rune(0xFF0F),
	rune(0xFF1A),
	rune(0xFF1B),
	rune(0xFF1F),
	rune(0xFF20),
	rune(0xFF3B),
	rune(0xFF3C),
	rune(0xFF3D),
	rune(0xFF3F),
	rune(0xFF5B),
	rune(0xFF5D),
	rune(0xFF5F),
	rune(0xFF60),
	rune(0xFF61),
	rune(0xFF62),
	rune(0xFF63),
	rune(0xFF64),
	rune(0xFF65),
	rune(0x10100),
	rune(0x10101),
	rune(0x10102),
	rune(0x1039F),
	rune(0x103D0),
	rune(0x1056F),
	rune(0x10857),
	rune(0x1091F),
	rune(0x1093F),
	rune(0x10A50),
	rune(0x10A51),
	rune(0x10A52),
	rune(0x10A53),
	rune(0x10A54),
	rune(0x10A55),
	rune(0x10A56),
	rune(0x10A57),
	rune(0x10A58),
	rune(0x10A7F),
	rune(0x10AF0),
	rune(0x10AF1),
	rune(0x10AF2),
	rune(0x10AF3),
	rune(0x10AF4),
	rune(0x10AF5),
	rune(0x10AF6),
	rune(0x10B39),
	rune(0x10B3A),
	rune(0x10B3B),
	rune(0x10B3C),
	rune(0x10B3D),
	rune(0x10B3E),
	rune(0x10B3F),
	rune(0x10B99),
	rune(0x10B9A),
	rune(0x10B9B),
	rune(0x10B9C),
	rune(0x10EAD),
	rune(0x10F55),
	rune(0x10F56),
	rune(0x10F57),
	rune(0x10F58),
	rune(0x10F59),
	rune(0x10F86),
	rune(0x10F87),
	rune(0x10F88),
	rune(0x10F89),
	rune(0x11047),
	rune(0x11048),
	rune(0x11049),
	rune(0x1104A),
	rune(0x1104B),
	rune(0x1104C),
	rune(0x1104D),
	rune(0x110BB),
	rune(0x110BC),
	rune(0x110BE),
	rune(0x110BF),
	rune(0x110C0),
	rune(0x110C1),
	rune(0x11140),
	rune(0x11141),
	rune(0x11142),
	rune(0x11143),
	rune(0x11174),
	rune(0x11175),
	rune(0x111C5),
	rune(0x111C6),
	rune(0x111C7),
	rune(0x111C8),
	rune(0x111CD),
	rune(0x111DB),
	rune(0x111DD),
	rune(0x111DE),
	rune(0x111DF),
	rune(0x11238),
	rune(0x11239),
	rune(0x1123A),
	rune(0x1123B),
	rune(0x1123C),
	rune(0x1123D),
	rune(0x112A9),
	rune(0x1144B),
	rune(0x1144C),
	rune(0x1144D),
	rune(0x1144E),
	rune(0x1144F),
	rune(0x1145A),
	rune(0x1145B),
	rune(0x1145D),
	rune(0x114C6),
	rune(0x115C1),
	rune(0x115C2),
	rune(0x115C3),
	rune(0x115C4),
	rune(0x115C5),
	rune(0x115C6),
	rune(0x115C7),
	rune(0x115C8),
	rune(0x115C9),
	rune(0x115CA),
	rune(0x115CB),
	rune(0x115CC),
	rune(0x115CD),
	rune(0x115CE),
	rune(0x115CF),
	rune(0x115D0),
	rune(0x115D1),
	rune(0x115D2),
	rune(0x115D3),
	rune(0x115D4),
	rune(0x115D5),
	rune(0x115D6),
	rune(0x115D7),
	rune(0x11641),
	rune(0x11642),
	rune(0x11643),
	rune(0x11660),
	rune(0x11661),
	rune(0x11662),
	rune(0x11663),
	rune(0x11664),
	rune(0x11665),
	rune(0x11666),
	rune(0x11667),
	rune(0x11668),
	rune(0x11669),
	rune(0x1166A),
	rune(0x1166B),
	rune(0x1166C),
	rune(0x116B9),
	rune(0x1173C),
	rune(0x1173D),
	rune(0x1173E),
	rune(0x1183B),
	rune(0x11944),
	rune(0x11945),
	rune(0x11946),
	rune(0x119E2),
	rune(0x11A3F),
	rune(0x11A40),
	rune(0x11A41),
	rune(0x11A42),
	rune(0x11A43),
	rune(0x11A44),
	rune(0x11A45),
	rune(0x11A46),
	rune(0x11A9A),
	rune(0x11A9B),
	rune(0x11A9C),
	rune(0x11A9E),
	rune(0x11A9F),
	rune(0x11AA0),
	rune(0x11AA1),
	rune(0x11AA2),
	rune(0x11B00),
	rune(0x11B01),
	rune(0x11B02),
	rune(0x11B03),
	rune(0x11B04),
	rune(0x11B05),
	rune(0x11B06),
	rune(0x11B07),
	rune(0x11B08),
	rune(0x11B09),
	rune(0x11C41),
	rune(0x11C42),
	rune(0x11C43),
	rune(0x11C44),
	rune(0x11C45),
	rune(0x11C70),
	rune(0x11C71),
	rune(0x11EF7),
	rune(0x11EF8),
	rune(0x11F43),
	rune(0x11F44),
	rune(0x11F45),
	rune(0x11F46),
	rune(0x11F47),
	rune(0x11F48),
	rune(0x11F49),
	rune(0x11F4A),
	rune(0x11F4B),
	rune(0x11F4C),
	rune(0x11F4D),
	rune(0x11F4E),
	rune(0x11F4F),
	rune(0x11FFF),
	rune(0x12470),
	rune(0x12471),
	rune(0x12472),
	rune(0x12473),
	rune(0x12474),
	rune(0x12FF1),
	rune(0x12FF2),
	rune(0x16A6E),
	rune(0x16A6F),
	rune(0x16AF5),
	rune(0x16B37),
	rune(0x16B38),
	rune(0x16B39),
	rune(0x16B3A),
	rune(0x16B3B),
	rune(0x16B44),
	rune(0x16E97),
	rune(0x16E98),
	rune(0x16E99),
	rune(0x16E9A),
	rune(0x16FE2),
	rune(0x1BC9F),
	rune(0x1DA87),
	rune(0x1DA88),
	rune(0x1DA89),
	rune(0x1DA8A),
	rune(0x1DA8B),
	rune(0x1E95E),
	rune(0x1E95F),
]
