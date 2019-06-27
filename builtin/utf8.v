// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

pub fn (s string) is_utf8() int {
	faulty_bytes := 0
	len := s.len
	i := 0
	// # size_t i = 0;
	# byte * str = s.str;
	# 
	# while (i < len) {
	# if (str[i] <= 0x7F) /* 00..7F */ {
	# i += 1;
	# }
#else if (str[i] >= 0xC2 && str[i] <= 0xDF) /* C2..DF 80..BF */ {
	# if (i + 1 < len) /* Expect a 2nd byte */ {
	# if (str[i + 1] < 0x80 || str[i + 1] > 0xBF) {
	# printf( "After a first byte between C2 and DF, expecting a 2nd byte between 80 and BF");
	# faulty_bytes = 2;
	# goto end;
	# }
	# }
#else {
	# printf( "After a first byte between C2 and DF, expecting a 2nd byte.");
	# faulty_bytes = 1;
	# goto end;
	# }
	# i += 2;
	# }
#else if (str[i] == 0xE0) /* E0 A0..BF 80..BF */ {
	# if (i + 2 < len) /* Expect a 2nd and 3rd byte */ {
	# if (str[i + 1] < 0xA0 || str[i + 1] > 0xBF) {
	# printf( "After a first byte of E0, expecting a 2nd byte between A0 and BF.");
	# faulty_bytes = 2;
	# goto end;
	# }
	# if (str[i + 2] < 0x80 || str[i + 2] > 0xBF) {
	# printf( "After a first byte of E0, expecting a 3nd byte between 80 and BF.");
	# faulty_bytes = 3;
	# goto end;
	# }
	# }
#else {
	# printf( "After a first byte of E0, expecting two following bytes.");
	# faulty_bytes = 1;
	# goto end;
	# }
	# i += 3;
	# }
#else if (str[i] >= 0xE1 && str[i] <= 0xEC) /* E1..EC 80..BF 80..BF */ {
	# if (i + 2 < len) /* Expect a 2nd and 3rd byte */ {
	# if (str[i + 1] < 0x80 || str[i + 1] > 0xBF) {
	# printf( "After a first byte between E1 and EC, expecting the 2nd byte between 80 and BF.");
	# faulty_bytes = 2;
	# goto end;
	# }
	# if (str[i + 2] < 0x80 || str[i + 2] > 0xBF) {
	# printf( "After a first byte between E1 and EC, expecting the 3rd byte between 80 and BF.");
	# faulty_bytes = 3;
	# goto end;
	# }
	# }
#else {
	# printf( "After a first byte between E1 and EC, expecting two following bytes.");
	# faulty_bytes = 1;
	# goto end;
	# }
	# i += 3;
	# }
#else if (str[i] == 0xED) /* ED 80..9F 80..BF */ {
	# if (i + 2 < len) /* Expect a 2nd and 3rd byte */ {
	# if (str[i + 1] < 0x80 || str[i + 1] > 0x9F) {
	# printf( "After a first byte of ED, expecting 2nd byte between 80 and 9F.");
	# faulty_bytes = 2;
	# goto end;
	# }
	# if (str[i + 2] < 0x80 || str[i + 2] > 0xBF) {
	# printf( "After a first byte of ED, expecting 3rd byte between 80 and BF.");
	# faulty_bytes = 3;
	# goto end;
	# }
	# }
#else {
	# printf( "After a first byte of ED, expecting two following bytes.");
	# faulty_bytes = 1;
	# goto end;
	# }
	# i += 3;
	# }
#else if (str[i] >= 0xEE && str[i] <= 0xEF) /* EE..EF 80..BF 80..BF */ {
	# if (i + 2 < len) /* Expect a 2nd and 3rd byte */ {
	# if (str[i + 1] < 0x80 || str[i + 1] > 0xBF) {
	# printf( "After a first byte between EE and EF, expecting 2nd byte between 80 and BF.");
	# faulty_bytes = 2;
	# goto end;
	# }
	# if (str[i + 2] < 0x80 || str[i + 2] > 0xBF) {
	# printf( "After a first byte between EE and EF, expecting 3rd byte between 80 and BF.");
	# faulty_bytes = 3;
	# goto end;
	# }
	# }
#else {
	# printf( "After a first byte between EE and EF, two following bytes.");
	# faulty_bytes = 1;
	# goto end;
	# }
	# i += 3;
	# }
#else if (str[i] == 0xF0) /* F0 90..BF 80..BF 80..BF */ {
	# if (i + 3 < len) /* Expect a 2nd, 3rd 3th byte */ {
	# if (str[i + 1] < 0x90 || str[i + 1] > 0xBF) {
	# printf( "After a first byte of F0, expecting 2nd byte between 90 and BF.");
	# faulty_bytes = 2;
	# goto end;
	# }
	# if (str[i + 2] < 0x80 || str[i + 2] > 0xBF) {
	# printf( "After a first byte of F0, expecting 3rd byte between 80 and BF.");
	# faulty_bytes = 3;
	# goto end;
	# }
	# if (str[i + 3] < 0x80 || str[i + 3] > 0xBF) {
	# printf( "After a first byte of F0, expecting 4th byte between 80 and BF.");
	# faulty_bytes = 4;
	# goto end;
	# }
	# }
#else {
	# printf( "After a first byte of F0, expecting three following bytes.");
	# faulty_bytes = 1;
	# goto end;
	# }
	# i += 4;
	# }
#else if (str[i] >= 0xF1 && str[i] <= 0xF3) /* F1..F3 80..BF 80..BF 80..BF */ {
	# if (i + 3 < len) /* Expect a 2nd, 3rd 3th byte */ {
	# if (str[i + 1] < 0x80 || str[i + 1] > 0xBF) {
	# printf( "After a first byte of F1, F2, or F3, expecting a 2nd byte between 80 and BF.");
	# faulty_bytes = 2;
	# goto end;
	# }
	# if (str[i + 2] < 0x80 || str[i + 2] > 0xBF) {
	# printf( "After a first byte of F1, F2, or F3, expecting a 3rd byte between 80 and BF.");
	# faulty_bytes = 3;
	# goto end;
	# }
	# if (str[i + 3] < 0x80 || str[i + 3] > 0xBF) {
	# printf( "After a first byte of F1, F2, or F3, expecting a 4th byte between 80 and BF.");
	# faulty_bytes = 4;
	# goto end;
	# }
	# }
#else {
	# printf( "After a first byte of F1, F2, or F3, expecting three following bytes.");
	# faulty_bytes = 1;
	# goto end;
	# }
	# i += 4;
	# }
#else if (str[i] == 0xF4) /* F4 80..8F 80..BF 80..BF */ {
	# if (i + 3 < len) /* Expect a 2nd, 3rd 3th byte */ {
	# if (str[i + 1] < 0x80 || str[i + 1] > 0x8F) {
	# printf( "After a first byte of F4, expecting 2nd byte between 80 and 8F.");
	# faulty_bytes = 2;
	# goto end;
	# }
	# if (str[i + 2] < 0x80 || str[i + 2] > 0xBF) {
	# printf( "After a first byte of F4, expecting 3rd byte between 80 and BF.");
	# faulty_bytes = 3;
	# goto end;
	# }
	# if (str[i + 3] < 0x80 || str[i + 3] > 0xBF) {
	# printf( "After a first byte of F4, expecting 4th byte between 80 and BF.");
	# faulty_bytes = 4;
	# goto end;
	# }
	# }
#else {
	# printf( "After a first byte of F4, expecting three following bytes.");
	# faulty_bytes = 1;
	# goto end;
	# }
	# i += 4;
	# }
#else {
	# printf( "i=%d Expecting bytes in the following ranges: 00..7F C2..F4.",
	# i);
	# faulty_bytes = 1;
	# goto end;
	# }
	# }
	# 
	# end: ;
	// println('faulty bytes=$faulty_bytes i=$i')
	// # printf("c='%c'\n", str[i]);
	ok := faulty_bytes == 0
	if ok {
		return -1
	}
	if !ok {
		println('utf is bad dalen=$len KEK $s sdf')
		// s = s.left(i)
	}
	return i
	// return ok
}

/* 
fn (s string) runes() []string {
	res2 := []string{}
	// res := new_empty_array_with_cap_string(s.len)
	res := []string{}
	if !s.is_utf8() {
		mys := s
		println('string.me runes bad utf $mys HAHA')
		return res
	}
	for i := 0; i < s.len; i++ {
		char_len := 0
		# char_len =UTF8_CHAR_LEN(s.str[i]);
		switch char_len {
		case 1:
			// println('ONE')
			res <<(char2string(s[i]))
		case 2:
			// println('TWO')
			rune2 := s.substr(i, i + 2)
			res <<(rune2)
			i++
		case 3:
			// println('TWO')
			rune3 := s.substr(i, i + 3)
			res <<(rune3)
			i++
			i++
		case 4:
			// println('TWO')
			rune4 := s.substr(i, i + 4)
			res <<(rune4)
			i++
			i++
			i++
		}
	}
	return res
}
*/
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

