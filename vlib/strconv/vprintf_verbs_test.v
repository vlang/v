import strconv

fn test_sprintf_octal_and_binary() {
	assert unsafe { strconv.v_sprintf('%o', 8) } == '10'
	assert unsafe { strconv.v_sprintf('%b', 5) } == '101'
	assert unsafe { strconv.v_sprintf('%o', 0) } == '0'
	assert unsafe { strconv.v_sprintf('%b', 0) } == '0'
	assert unsafe { strconv.v_sprintf('%o %b', 511, 255) } == '777 11111111'
}

fn test_sprintf_octal_and_binary_argument_types() {
	a := u8(200)
	b := i16(13)
	c := u16(0xffff)
	d := u32(0xffff_ffff)
	e := i64(0x1_0000_0000)
	f := u64(0xffff_ffff_ffff_ffff)
	// `u8`, `i16` and `u16` arguments are promoted to `int`, like in C
	assert unsafe { strconv.v_sprintf('%o %hho %hhb', a, a, a) } == '310 310 11001000'
	assert unsafe { strconv.v_sprintf('%o %ho %b', b, b, b) } == '15 15 1101'
	assert unsafe { strconv.v_sprintf('%o %ho', c, c) } == '177777 177777'
	assert unsafe { strconv.v_sprintf('%o', d) } == '37777777777'
	assert unsafe { strconv.v_sprintf('%b', d) } == '1'.repeat(32)
	assert unsafe { strconv.v_sprintf('%lo %lb', e, e) } == '40000000000 1' + '0'.repeat(32)
	assert unsafe { strconv.v_sprintf('%lo %llo', f, f) } == '1' + '7'.repeat(21) + ' 1' +
		'7'.repeat(21)
	assert unsafe { strconv.v_sprintf('%lb', f) } == '1'.repeat(64)
}

fn test_sprintf_octal_and_binary_of_negative_values() {
	// two's complement, at the width that the length field selects
	assert unsafe { strconv.v_sprintf('%o', -1) } == '37777777777'
	assert unsafe { strconv.v_sprintf('%b', -1) } == '1'.repeat(32)
	assert unsafe { strconv.v_sprintf('%b', -2) } == '1'.repeat(31) + '0'
	assert unsafe { strconv.v_sprintf('%hho %hhb', -1, -2) } == '377 11111110'
	assert unsafe { strconv.v_sprintf('%ho %hb', -1, -2) } == '177777 1111111111111110'
	assert unsafe { strconv.v_sprintf('%o', i8(-2)) } == '37777777776'
	assert unsafe { strconv.v_sprintf('%hho', i8(-2)) } == '376'
	assert unsafe { strconv.v_sprintf('%o', i32(-8)) } == '37777777770'
	assert unsafe { strconv.v_sprintf('%lo', i64(-1)) } == '1' + '7'.repeat(21)
	assert unsafe { strconv.v_sprintf('%lb', i64(-1)) } == '1'.repeat(64)
	assert unsafe { strconv.v_sprintf('%lb', min_i64) } == '1' + '0'.repeat(63)
}

fn test_sprintf_octal_and_binary_width() {
	assert unsafe { strconv.v_sprintf('[%6o] [%-6o] [%06o]', 8, 8, 8) } == '[    10] [10    ] [000010]'
	assert unsafe { strconv.v_sprintf('[%6b] [%-6b] [%06b]', 5, 5, 5) } == '[   101] [101   ] [000101]'
	assert unsafe { strconv.v_sprintf('[%2o] [%2b]', 511, 5) } == '[777] [101]'
	assert unsafe { strconv.v_sprintf('[%5hho] [%-12lb]', -1, i64(5)) } == '[  377] [101         ]'
}

fn test_sprintf_alternative_form() {
	assert unsafe { strconv.v_sprintf('%#x', 255) } == '0xff'
	assert unsafe { strconv.v_sprintf('%#X', 255) } == '0XFF'
	assert unsafe { strconv.v_sprintf('%#o', 8) } == '010'
	assert unsafe { strconv.v_sprintf('%#b', 5) } == '0b101'
	// like in C, a zero value gets no prefix
	assert unsafe { strconv.v_sprintf('%#x %#X %#o %#b', 0, 0, 0, 0) } == '0 0 0 0'
	assert unsafe { strconv.v_sprintf('%#lx %#lo %#lb', i64(0), i64(0), i64(0)) } == '0 0 0'
	assert unsafe { strconv.v_sprintf('%#o %#b', -1, -1) } == '037777777777 0b' + '1'.repeat(32)
	assert unsafe { strconv.v_sprintf('%#hhx %#hx %#lx', u8(200), u16(0xabc), u64(0xffff_ffff_ffff_ffff)) } == '0xc8 0xabc 0xffffffffffffffff'
	assert unsafe { strconv.v_sprintf('%#hho %#lb', u8(200), i64(5)) } == '0310 0b101'
}

fn test_sprintf_alternative_form_width() {
	assert unsafe { strconv.v_sprintf('[%#8x] [%-#8x] [%#-8X]', 255, 255, 255) } == '[    0xff] [0xff    ] [0XFF    ]'
	assert unsafe { strconv.v_sprintf('[%#8o] [%-#8o]', 8, 8) } == '[     010] [010     ]'
	assert unsafe { strconv.v_sprintf('[%#8b] [%-#8b]', 5, 5) } == '[   0b101] [0b101   ]'
	// the zeros go between the prefix and the digits
	assert unsafe { strconv.v_sprintf('[%#08x] [%0#8X]', 255, 255) } == '[0x0000ff] [0X0000FF]'
	assert unsafe { strconv.v_sprintf('[%#06o] [%#03o] [%#02o]', 8, 8, 8) } == '[000010] [010] [010]'
	assert unsafe { strconv.v_sprintf('[%#010b] [%#04b]', 5, 5) } == '[0b00000101] [0b101]'
	assert unsafe { strconv.v_sprintf('[%#08x] [%#04o] [%#4b]', 0, 0, 0) } == '[00000000] [0000] [   0]'
}

fn test_sprintf_alternative_form_is_reset_and_leaves_other_verbs_alone() {
	assert unsafe { strconv.v_sprintf('%#x %x %#o %o %#b %b', 255, 255, 8, 8, 5, 5) } == '0xff ff 010 10 0b101 101'
	assert unsafe { strconv.v_sprintf('%#d %#u %#s %d', -5, 7, 'abc', 9) } == '-5 7 abc 9'
}

fn test_sprintf_bool() {
	yes := true
	no := false
	assert unsafe { strconv.v_sprintf('%t', true) } == 'true'
	assert unsafe { strconv.v_sprintf('%t', false) } == 'false'
	assert unsafe { strconv.v_sprintf('%t %t', yes, no) } == 'true false'
	assert unsafe { strconv.v_sprintf('%t %t', 2 > 1, yes && no) } == 'true false'
	assert unsafe { strconv.v_sprintf('[%6t] [%-6t] [%2t]', yes, no, yes) } == '[  true] [false ] [true]'
}

fn test_sprintf_quoted_string() {
	assert unsafe { strconv.v_sprintf('%q', 'hi') } == '"hi"'
	assert unsafe { strconv.v_sprintf('%q', '') } == '""'
	assert unsafe { strconv.v_sprintf('%q', 'a"b\\c') } == r'"a\"b\\c"'
	assert unsafe { strconv.v_sprintf('%q', "it's") } == '"it\'s"'
	assert unsafe { strconv.v_sprintf('%q', 'a\nb\tc\rd') } == r'"a\nb\tc\rd"'
	assert unsafe { strconv.v_sprintf('%q', '\a\b\f\v') } == r'"\a\b\f\v"'
	assert unsafe { strconv.v_sprintf('%q', '\x00\x01\x1f\x7f') } == r'"\x00\x01\x1f\x7f"'
	// valid UTF-8 is kept, an invalid byte is escaped
	assert unsafe { strconv.v_sprintf('%q', 'café 日本語') } == '"café 日本語"'
	assert unsafe { strconv.v_sprintf('%q', [u8(`a`), 0xff].bytestr()) } == r'"a\xff"'
	for s in ['plain', 'tab\there', 'quote"d', ' ', '😀'] {
		assert unsafe { strconv.v_sprintf('%q', s) } == strconv.quote(s)
	}
}

fn test_sprintf_quoted_string_width() {
	assert unsafe { strconv.v_sprintf('[%8q] [%-8q] [%2q]', 'hi', 'hi', 'hi') } == '[    "hi"] ["hi"    ] ["hi"]'
	// the width counts characters, not bytes
	assert unsafe { strconv.v_sprintf('[%6q]', 'né') } == '[  "né"]'
	assert unsafe { strconv.v_sprintf('[%6q]', 'a\n') } == r'[ "a\n"]'
}

fn test_sprintf_new_verbs_keep_arguments_in_step() {
	assert unsafe { strconv.v_sprintf('%d %o %b %t %q %s %#x %d', 1, 8, 5, true, 'q', 's', 255, 2) } == '1 10 101 true "q" s 0xff 2'
}

fn test_sprintf_unknown_verb_is_written_back() {
	// it is not dropped, and it still takes one argument
	assert unsafe { strconv.v_sprintf('%y', 1) } == '%y'
	assert unsafe { strconv.v_sprintf('a%yb%dc', 1, 2) } == 'a%yb2c'
	assert unsafe { strconv.v_sprintf('%d %k %s', 1, 2, 'x') } == '1 %k x'
	assert unsafe { strconv.v_sprintf('[%-08.3ly] [%+hhv]', 1, 2) } == '[%-08.3ly] [%+hhv]'
	assert unsafe { strconv.v_sprintf('100%% %y %%', 1) } == '100% %y %'
	// its flags do not leak into the next specifier
	assert unsafe { strconv.v_sprintf('[%#-08y] [%x] [%4d]', 1, 255, 7) } == '[%#-08y] [ff] [   7]'
	assert unsafe { strconv.v_sprintf('%é|%d', 1, 2) } == '%é|2'
}
