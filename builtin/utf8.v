// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

fn (s string) is_utf8() int {
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
fn utf32_to_str(code u32) string {
	// println('code = $code')
	buffer := malloc(5)
	# if (code <= 0x7F) {
	// println('!!!!!!!1')
	# buffer[0] = code;
	# return tos(buffer, 1);
	# }
	# if (code <= 0x7FF) {
	// println('!!!!!!!2')
	# buffer[0] = 0xC0 | (code >> 6);            /* 110xxxxx */
	# buffer[1] = 0x80 | (code & 0x3F);          /* 10xxxxxx */
	# return tos(buffer, 2);
	# }
	# if (code <= 0xFFFF) {
	// println('!!!!!!!3')
	# buffer[0] = 0xE0 | (code >> 12);           /* 1110xxxx */
	# buffer[1] = 0x80 | ((code >> 6) & 0x3F);   /* 10xxxxxx */
	# buffer[2] = 0x80 | (code & 0x3F);          /* 10xxxxxx */
	# return tos(buffer, 3);
	# }
	# if (code <= 0x10FFFF) {
	# buffer[0] = 0xF0 | (code >> 18);           /* 11110xxx */
	# buffer[1] = 0x80 | ((code >> 12) & 0x3F);  /* 10xxxxxx */
	# buffer[2] = 0x80 | ((code >> 6) & 0x3F);   /* 10xxxxxx */
	# buffer[3] = 0x80 | (code & 0x3F);          /* 10xxxxxx */
	# return tos(buffer, 4);
	# }
	return ''
}

// TODO copypasta
fn utf32_to_str_no_malloc(code u32, buf voidptr) string {
	// println('code = $code')
	# char* buffer = buf;
	# if (code <= 0x7F) {
	// println('!!!!!!!1')
	# buffer[0] = code;
	# return tos(buffer, 1);
	# }
	# if (code <= 0x7FF) {
	// println('!!!!!!!2')
	# buffer[0] = 0xC0 | (code >> 6);            /* 110xxxxx */
	# buffer[1] = 0x80 | (code & 0x3F);          /* 10xxxxxx */
	# return tos(buffer, 2);
	# }
	# if (code <= 0xFFFF) {
	// println('!!!!!!!3')
	# buffer[0] = 0xE0 | (code >> 12);           /* 1110xxxx */
	# buffer[1] = 0x80 | ((code >> 6) & 0x3F);   /* 10xxxxxx */
	# buffer[2] = 0x80 | (code & 0x3F);          /* 10xxxxxx */
	# return tos(buffer, 3);
	# }
	# if (code <= 0x10FFFF) {
	# buffer[0] = 0xF0 | (code >> 18);           /* 11110xxx */
	# buffer[1] = 0x80 | ((code >> 12) & 0x3F);  /* 10xxxxxx */
	# buffer[2] = 0x80 | ((code >> 6) & 0x3F);   /* 10xxxxxx */
	# buffer[3] = 0x80 | (code & 0x3F);          /* 10xxxxxx */
	# return tos(buffer, 4);
	# }
	return ''
}

// Convert utf8 to utf32
fn (_rune string) utf32_code() int {
	// println('utf 32 of $rune len=$rune.len')
	if _rune.len == 0 {
		return 0
	}
	// save ASC symbol as is
	if _rune.len == 1 {
		return int(_rune[0])
	}
	b := byte(int(_rune[0]))
	// TODO should be
	// res := int( rune[0] << rune.len)
	# b <<= _rune.len;
	res := int(b)
	mut shift := 6 - _rune.len
	for i := 1; i < _rune.len; i++ {
		// println('c=$res')
		c := int(_rune[i])
		# res <<= shift;
		# res |= c & 0x3f;
		shift = 6
	}
	// println('!!!!!!!! utf32 $rune res = $res')
	return res
}

