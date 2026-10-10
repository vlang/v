module term

fn test_strip_ansi_removes_a_csi_sequence_ending_in_a_final_byte() {
	assert strip_ansi('\x1b[2Jabc') == 'abc'
	assert strip_ansi('\x1b[?25labc') == 'abc'
	assert strip_ansi('\x1b[1;1Hxy') == 'xy'
	assert strip_ansi('a\x1b[0mb\x1b[0mc') == 'abc'
}

fn test_strip_ansi_removes_an_osc_sequence() {
	assert strip_ansi('\x1b]0;title\x07abc') == 'abc'
	assert strip_ansi('\x1b]0;title\x1b\\abc') == 'abc'
}

fn test_strip_ansi_removes_a_charset_selection_prefix() {
	assert strip_ansi('\x1bMabc') == 'abc'
	assert strip_ansi('\x1b%Gabc') == 'abc'
}

fn test_strip_ansi_leaves_an_unterminated_osc_nothing_to_emit() {
	// the rest of the string is part of the sequence, since it is never ended
	assert strip_ansi('\x1b]0;12abc') == ''
}

// NOTE: `\x1b(B` is a complete charset selection escape, but strip_ansi only
// consumes the ESC and the `(`, so the following byte survives. printable_len
// skips all three bytes, so the two disagree on this input.
fn test_strip_ansi_keeps_the_byte_after_an_unrecognised_escape() {
	assert strip_ansi('\x1b(Babc') == 'Babc'
	assert strip_ansi('\x1bXabc') == 'abc'
}

fn test_strip_ansi_leaves_plain_text_alone() {
	assert strip_ansi('') == ''
	assert strip_ansi('plain text') == 'plain text'
	assert strip_ansi('a\x1b') == 'a'
	assert strip_ansi('abc\x1b[') == 'abc'
	assert strip_ansi('\x1b[') == ''
}

fn test_strip_ansi_removes_the_codes_of_every_colour_helper() {
	messages := [red('x'), bg_blue('x'), bold(italic('x')), underline('x'),
		strikethrough(inverse(dim('x'))), rgb(1, 2, 3, 'x'), bg_rgb(4, 5, 6, 'x'), hex(0x010203, 'x'),
		bg_hex(0x040506, 'x'), reset('x'), gray('x'), bright_white(bright_bg_black('x')), failed('x')]
	for s in messages {
		assert strip_ansi(s) == 'x', s
	}
}

fn test_strip_ansi_keeps_the_padding_around_a_highlighted_command() {
	assert strip_ansi(highlight_command('v run')) == ' v run '
}
