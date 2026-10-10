module term

fn test_h_divider_spans_the_terminal_width() {
	cols, _ := get_terminal_size()
	assert h_divider('-').len == cols
	assert h_divider('').len == cols
	assert h_divider('abc').len == cols
	assert h_divider('=_').len == cols
}

fn test_h_divider_of_a_single_character_divider_repeats_that_character() {
	line := h_divider('-')
	assert line == '-'.repeat(line.len)
}

fn test_h_divider_of_an_empty_divider_is_spaces() {
	cols, _ := get_terminal_size()
	assert h_divider('') == ' '.repeat(cols)
}

fn test_h_divider_of_a_multi_character_divider_repeats_the_whole_unit() {
	cols, _ := get_terminal_size()
	if cols < 6 {
		return
	}
	line := h_divider('abc')
	assert line.contains('abcabc')
	assert line[..cols - cols % 3] == 'abc'.repeat(cols / 3)
}

fn test_header_of_an_empty_text_is_the_divider_line() {
	assert header('', '-') == h_divider('-')
	assert header('', '') == h_divider('')
}

fn test_header_centres_the_text_between_divider_characters() {
	cols, _ := get_terminal_size()
	line := header('TEXT', '=')
	assert line.len == cols
	start := line.index(' TEXT ') or {
		assert false, 'header does not centre TEXT: ${line}'
		return
	}
	assert line[..start].trim('=') == ''
	assert line[start + 6..].trim('=') == ''
}

fn test_header_truncates_a_text_that_is_wider_than_the_terminal() {
	cols, _ := get_terminal_size()
	long_text := '0123456789'.repeat(20)
	line := header(long_text, '-')
	assert line.len == cols
	assert !line.contains(long_text)
}

fn test_header_left_puts_a_four_character_divider_before_the_text() {
	cols, _ := get_terminal_size()
	line := header_left('TITLE', '=')
	assert line.len == cols
	assert line.starts_with('==== TITLE ')
	assert line[4] == ` `
	assert line.ends_with('=')
	assert line.trim('=') == ' TITLE '
}

fn test_header_left_of_an_empty_divider_uses_spaces() {
	cols, _ := get_terminal_size()
	line := header_left('TITLE', '')
	assert line.len == cols
	assert line.starts_with('     TITLE ')
	assert line.trim(' ') == 'TITLE'
}

fn test_header_left_measures_the_plain_text_but_prints_the_marked_up_one() {
	plain := header_left('TITLE', '=')
	marked := header_left(bold('TITLE'), '=')
	// the escape codes are not part of the width calculation
	assert marked.len == plain.len + 9
	assert marked.contains(bold('TITLE'))
}
