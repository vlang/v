import strconv

fn test_space_flag_stands_for_the_sign_of_a_non_negative_number() {
	unsafe {
		assert strconv.v_sprintf('% d', 42) == ' 42'
		assert strconv.v_sprintf('% d', 0) == ' 0'
		assert strconv.v_sprintf('% d', -42) == '-42'
		assert strconv.v_sprintf('% i', 42) == ' 42'
		assert strconv.v_sprintf('a% db% dc', 1, -2) == 'a 1b-2c'
		// the flag does not leak into the next specifier
		assert strconv.v_sprintf('% d %d', 1, 2) == ' 1 2'
		// `+` wins over ` `
		assert strconv.v_sprintf('%+ d', 42) == '+42'
		assert strconv.v_sprintf('% +d', 42) == '+42'
		assert strconv.v_sprintf('%+d', 42) == '+42'
		// length modifiers
		assert strconv.v_sprintf('% hhd', i8(3)) == ' 3'
		assert strconv.v_sprintf('% hhd', i8(-3)) == '-3'
		assert strconv.v_sprintf('% hd', i16(3)) == ' 3'
		assert strconv.v_sprintf('% ld', i64(42)) == ' 42'
		assert strconv.v_sprintf('% lld', i64(-42)) == '-42'
	}
}

fn test_space_flag_is_a_part_of_the_width() {
	unsafe {
		assert strconv.v_sprintf('[% 5d]', 42) == '[   42]'
		assert strconv.v_sprintf('[% 5d]', -42) == '[  -42]'
		assert strconv.v_sprintf('[% -5d]', 42) == '[ 42  ]'
		assert strconv.v_sprintf('[%- 5d]', 42) == '[ 42  ]'
		assert strconv.v_sprintf('[% 05d]', 42) == '[ 0042]'
		assert strconv.v_sprintf('[%0 5d]', 42) == '[ 0042]'
		assert strconv.v_sprintf('[% 05d]', -42) == '[-0042]'
		assert strconv.v_sprintf('[% 2d]', 12345) == '[ 12345]'
		// without the flag, nothing changes
		assert strconv.v_sprintf('[%5d]', 42) == '[   42]'
		assert strconv.v_sprintf('[%05d]', 42) == '[00042]'
		assert strconv.v_sprintf('[%-5d]', 42) == '[42   ]'
	}
}

fn test_space_flag_with_floats() {
	unsafe {
		assert strconv.v_sprintf('% f', 1.5) == ' 1.500000'
		assert strconv.v_sprintf('% f', -1.5) == '-1.500000'
		assert strconv.v_sprintf('% F', 1.5) == ' 1.500000'
		assert strconv.v_sprintf('%+ f', 1.5) == '+1.500000'
		assert strconv.v_sprintf('[% 10.2f]', 1.5) == '[      1.50]'
		assert strconv.v_sprintf('[% 010.2f]', 1.5) == '[ 000001.50]'
		assert strconv.v_sprintf('[% -10.2f]', 1.5) == '[ 1.50     ]'
		assert strconv.v_sprintf('% e', 1.5) == ' 1.500000e+00'
		assert strconv.v_sprintf('% E', -1.5) == '-1.500000E+00'
		assert strconv.v_sprintf('[% 14.3e]', 1.5) == '[     1.500e+00]'
		assert strconv.v_sprintf('% g', 1.5) == ' 1.5'
		assert strconv.v_sprintf('% g', -1.5) == '-1.5'
		assert strconv.v_sprintf('% g', 1.5e20) == ' 1.5e+20'
		assert strconv.v_sprintf('[% 8g]', 1.5) == '[     1.5]'
	}
}

fn test_space_flag_only_signs() {
	unsafe {
		// an unsigned number, a string and a character have no sign to stand for
		assert strconv.v_sprintf('% u', 42) == '42'
		assert strconv.v_sprintf('% x', 42) == '2a'
		assert strconv.v_sprintf('% s', 'ab') == 'ab'
		assert strconv.v_sprintf('[% 5s]', 'ab') == '[   ab]'
		assert strconv.v_sprintf('% c', `A`) == 'A'
		// and it is not a padding character, that could undo a `0`
		assert strconv.v_sprintf('[%0 5x]', 42) == '[0002a]'
	}
}

// `%x` shows the two's complement of a negative number, at the width that the length
// field gives, like C does. `v_sprintf` can not show `-1` instead, like Go does for a
// signed operand: all it gets is a pointer, and `u32(0xffff_ffff)` is the same bits.
fn test_hex_of_a_negative_number_has_the_width_of_the_length_field() {
	unsafe {
		assert strconv.v_sprintf('%x', -1) == 'ffffffff'
		assert strconv.v_sprintf('%X', -255) == 'FFFFFF01'
		assert strconv.v_sprintf('%hhx', i8(-1)) == 'ff'
		assert strconv.v_sprintf('%hhx', i8(-128)) == '80'
		assert strconv.v_sprintf('%hhx', -1) == 'ff'
		assert strconv.v_sprintf('%hx', i16(-1)) == 'ffff'
		assert strconv.v_sprintf('%hx', i16(-2)) == 'fffe'
		assert strconv.v_sprintf('%lx', i64(-1)) == 'ffffffffffffffff'
		assert strconv.v_sprintf('%llx', i64(-2)) == 'fffffffffffffffe'
		assert strconv.v_sprintf('%lX', min_i64) == '8000000000000000'
		assert strconv.v_sprintf('[%5x]', -1) == '[ffffffff]'
		assert strconv.v_sprintf('[%-5x]', -1) == '[ffffffff]'
		assert strconv.v_sprintf('[%20lx]', i64(-1)) == '[    ffffffffffffffff]'
		assert strconv.v_sprintf('[%5x]', 255) == '[   ff]'
		assert strconv.v_sprintf('[%-5x]', 255) == '[ff   ]'
	}
}

fn test_hex_keeps_all_64_bits_only_with_a_long_length_field() {
	unsafe {
		assert strconv.v_sprintf('%lx', i64(0x1234567890abcdef)) == '1234567890abcdef'
		assert strconv.v_sprintf('%llx', i64(0x1234567890abcdef)) == '1234567890abcdef'
		assert strconv.v_sprintf('%lX', u64(0xfedcba9876543210)) == 'FEDCBA9876543210'
		assert strconv.v_sprintf('%lx', i64(0x1_0000_0000)) == '100000000'
		// without it, `%x` is the 32 bits of a C `unsigned int`
		assert strconv.v_sprintf('%x', i64(0x1234567890abcdef)) == '90abcdef'
	}
}

fn test_char_is_written_as_utf8() {
	unsafe {
		assert strconv.v_sprintf('%c', rune(233)).bytes() == [u8(0xc3), 0xa9]
		assert strconv.v_sprintf('%c', rune(233)) == 'é'
		assert strconv.v_sprintf('%c', `é`) == 'é'
		assert strconv.v_sprintf('%c', `€`).bytes() == [u8(0xe2), 0x82, 0xac]
		assert strconv.v_sprintf('%c', `€`) == '€'
		assert strconv.v_sprintf('%c', `😀`).bytes() == [u8(0xf0), 0x9f, 0x98, 0x80]
		assert strconv.v_sprintf('%c', `😀`) == '😀'
		assert strconv.v_sprintf('[%c%c%c%c]', `a`, `é`, `€`, `😀`) == '[aé€😀]'
		r := `ñ`
		assert strconv.v_sprintf('%c|%d', r, 7) == 'ñ|7'
	}
}

fn test_char_takes_a_rune_or_a_byte() {
	unsafe {
		assert strconv.v_sprintf('%c', `A`) == 'A'
		assert strconv.v_sprintf('%c', u8(65)) == 'A'
		assert strconv.v_sprintf('%c', 65) == 'A'
		b := u8(`z`)
		assert strconv.v_sprintf('%c', b) == 'z'
		assert strconv.v_sprintf('%c', 'hello'[1]) == 'e'
		assert strconv.v_sprintf('%c%c|%c', u8(97), `b`, 99) == 'ab|c'
		assert strconv.v_sprintf('%c', u8(0)).bytes() == [u8(0)]
		// a byte is a code point too, never a part of an UTF-8 sequence
		assert strconv.v_sprintf('%c', u8(233)) == 'é'
	}
}

fn test_char_that_is_not_a_code_point_is_replaced() {
	unsafe {
		replacement := [u8(0xef), 0xbf, 0xbd] // U+FFFD
		assert strconv.v_sprintf('%c', 0x110000).bytes() == replacement
		assert strconv.v_sprintf('%c', 0xd800).bytes() == replacement
		assert strconv.v_sprintf('%c', 0xdfff).bytes() == replacement
		assert strconv.v_sprintf('%c', -1).bytes() == replacement
		assert strconv.v_sprintf('%c', 0xd7ff).bytes() == [u8(0xed), 0x9f, 0xbf]
		assert strconv.v_sprintf('%c', 0x10ffff).bytes() == [u8(0xf4), 0x8f, 0xbf, 0xbf]
	}
}

fn test_string_precision() {
	unsafe {
		assert strconv.v_sprintf('%.3s', 'abcdef') == 'abc'
		assert strconv.v_sprintf('%.1s', 'abcdef') == 'a'
		assert strconv.v_sprintf('%.6s', 'abcdef') == 'abcdef'
		assert strconv.v_sprintf('%.10s', 'abc') == 'abc'
		assert strconv.v_sprintf('%.12s', 'abcdefghijklmnop') == 'abcdefghijkl'
		assert strconv.v_sprintf('%.0s', 'abcdef') == ''
		assert strconv.v_sprintf('%.3s', '') == ''
		// with a width
		assert strconv.v_sprintf('[%5.3s]', 'abcdef') == '[  abc]'
		assert strconv.v_sprintf('[%-5.3s]', 'abcdef') == '[abc  ]'
		assert strconv.v_sprintf('[%2.3s]', 'abcdef') == '[abc]'
		assert strconv.v_sprintf('[%4.0s]', 'abcdef') == '[    ]'
		// the precision does not leak into the next specifier, and the argument is not changed
		s := 'abcdef'
		assert strconv.v_sprintf('%.2s %s %.4s', s, s, s) == 'ab abcdef abcd'
		assert s == 'abcdef'
		// no precision
		assert strconv.v_sprintf('%s', 'abcdef') == 'abcdef'
		assert strconv.v_sprintf('[%8s]', 'abc') == '[     abc]'
		assert strconv.v_sprintf('[%-8s]', 'abc') == '[abc     ]'
	}
}

fn test_string_precision_counts_runes() {
	unsafe {
		assert strconv.v_sprintf('%.2s', 'héllo') == 'hé'
		assert strconv.v_sprintf('%.2s', 'héllo').bytes() == [u8(`h`), 0xc3, 0xa9]
		assert strconv.v_sprintf('%.3s', '日本語テキスト') == '日本語'
		assert strconv.v_sprintf('%.1s', '😀😀') == '😀'
		assert strconv.v_sprintf('%.9s', 'héllo') == 'héllo'
		assert strconv.v_sprintf('[%5.2s]', 'héllo') == '[   hé]'
		assert strconv.v_sprintf('[%-5.2s]', 'héllo') == '[hé   ]'
		// a sequence that is cut short by the end of the string, is not read past that end
		assert strconv.v_sprintf('%.2s', 'a\xe6').bytes() == [u8(`a`), 0xe6]
	}
}

fn test_string_star_precision_is_the_same() {
	unsafe {
		assert strconv.v_sprintf('%.*s', 3, 'abcdef') == 'abc'
		assert strconv.v_sprintf('%.*s', 0, 'abcdef') == ''
		assert strconv.v_sprintf('%.*s', 2, 'héllo') == 'hé'
		assert strconv.v_sprintf('%.*s', 10, 'abc') == 'abc'
		assert strconv.v_sprintf('%.*s', -1, 'abc') == 'abc'
		assert strconv.v_sprintf('%.*s|%d', 2, 'abcdef', 7) == 'ab|7'
	}
}

fn test_trailing_percent_is_kept() {
	unsafe {
		assert strconv.v_sprintf('100%') == '100%'
		assert strconv.v_sprintf('%') == '%'
		assert strconv.v_sprintf('a%') == 'a%'
		assert strconv.v_sprintf('%d%', 5) == '5%'
		assert strconv.v_sprintf('%s is 100%', 'it') == 'it is 100%'
		assert strconv.v_sprintf('%%%') == '%%'
		// `%%` is still one `%`
		assert strconv.v_sprintf('%%') == '%'
		assert strconv.v_sprintf('100%%') == '100%'
		assert strconv.v_sprintf('%d%%', 5) == '5%'
		assert strconv.v_sprintf('%%%d', 5) == '%5'
	}
}

fn test_format_that_ends_inside_of_a_specifier_keeps_it() {
	unsafe {
		assert strconv.v_sprintf('a%5') == 'a%5'
		assert strconv.v_sprintf('a%-') == 'a%-'
		assert strconv.v_sprintf('a%+08') == 'a%+08'
		assert strconv.v_sprintf('a%.3') == 'a%.3'
		assert strconv.v_sprintf('a%l') == 'a%l'
		assert strconv.v_sprintf('%d%5.2', 1) == '1%5.2'
	}
}
