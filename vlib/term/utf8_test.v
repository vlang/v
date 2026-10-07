module term

fn test_printable_len_graphemes() {
	// plain ASCII
	assert printable_len('') == 0
	assert printable_len('abc') == 3

	// wide CJK
	assert printable_len('世界') == 4

	// combining mark
	assert printable_len('\u006E\u0303') == 1

	// ZWJ emoji sequences
	assert printable_len('\U0001F3F3\uFE0F\u200D\U0001F308') == 2 // flag + ZWJ + rainbow
	assert printable_len('👩🏽‍💻') == 2 // woman + skin tone + ZWJ + laptop

	// Thai with combining vowels / tone marks
	assert printable_len('ห์') == 1
	assert printable_len('ปีเตอร์') == 5
}

fn test_printable_len_ansi_sgr() {
	assert printable_len('\x1b[31mred\x1b[0m') == 3
	assert printable_len('\x1b[1;32mbold green\x1b[0m') == 10
	assert printable_len('a\x1b[0mb\x1b[0mc') == 3

	// only escapes
	assert printable_len('\x1b[31m') == 0
	assert printable_len('\x1b[0m\x1b[1m') == 0
}

fn test_printable_len_ansi_osc() {
	// OSC 8 hyperlink, BEL-terminated
	assert printable_len('\x1b]8;;https://example.com\x07link\x1b]8;;\x07') == 4

	// OSC 8 hyperlink, ST-terminated
	assert printable_len('\x1b]8;;https://example.com\x1b\\link\x1b]8;;\x1b\\') == 4
}

fn test_printable_len_ansi_other() {
	// two-byte escapes
	assert printable_len('\x1bMabc') == 3 // reverse index
	assert printable_len('\x1b7abc\x1b8') == 3 // save/restore cursor

	// CSI with non-SGR final byte
	assert printable_len('\x1b[2Jhello') == 5 // clear screen
	assert printable_len('\x1b[1;1Hxy') == 2 // cursor home

	// DCS
	assert printable_len('\x1bPq#0;2;0;0;0\x1b\\abc') == 3

	// APC / PM
	assert printable_len('\x1b_apc\x1b\\xy') == 2
	assert printable_len('\x1b^pm\x1b\\xy') == 2
}

fn test_printable_len_ansi_inside_cluster() {
	// escape between base char and combining mark
	assert printable_len('\u006E\x1b[31m\u0303\x1b[0m') == 1

	// escape between ZWJ components
	assert printable_len('\U0001F3F3\x1b[0m\uFE0F\u200D\U0001F308') == 2

	// escape before a wide char
	assert printable_len('\x1b[31m世\x1b[0m') == 2
}

fn test_printable_len_unterminated_ansi() {
	// unterminated sequences consume to end; no crash
	assert printable_len('abc\x1b[') == 3
	assert printable_len('abc\x1b]8;;url') == 3
	assert printable_len('\x1b') == 0
	assert printable_len('\x1b[') == 0
}

fn test_printable_len_ansi_intermediate_bytes() {
	assert printable_len('\x1b(Babc') == 3
	assert printable_len('\x1b#8abc') == 3
	assert printable_len('abc\x1b(') == 3
	assert printable_len('\x1b(') == 0
}

fn test_printable_len_uses_existing_string_width_rules() {
	for text in ['a\n', '\r\n', '\t', '🇦🇺'] {
		assert printable_len('\x1b[31m' + text + '\x1b[0m') == utf8_str_visible_length(text)
	}
}
