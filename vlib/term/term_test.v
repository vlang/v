import term

fn test_get_terminal_size() {
	cols,_ := term.get_terminal_size()
	assert cols > 0
}

fn test_h_divider() {
	divider := term.h_divider('-')
	assert divider.len > 0
	assert divider[0] == `-`
	assert divider[divider.len - 1] == `-`
}

fn test_h_divider_multiple_characters() {
	xdivider := term.h_divider('abc')
	assert xdivider.len > 0
	assert xdivider.contains('abcabc')
}

fn test_header() {
	divider := term.h_divider('-')
	term_width := divider.len
	assert term_width > 0
	empty_header := term.header('', '-')
	short_header := term.header('reasonable header', '-')
	very_long_header := term.header(['abc'].repeat(500).join(' '), '-')
	very_long_header_2 := term.header(['abcd'].repeat(500).join(' '), '-')
	/*
	eprintln(divider)
	eprintln(empty_header)
	eprintln(short_header)
	eprintln(term.header('another longer header', '_-/\\'))
	eprintln(term.header('another longer header', '-'))
	eprintln(term.header('short', '-'))
	eprintln(term.header('12345', '-'))
	eprintln(term.header('1234', '-'))
	eprintln(term.header('123', '-'))
	eprintln(term.header('12', '-'))
	eprintln(term.header('1', '-'))
	eprintln(very_long_header)
	eprintln(divider)
	eprintln(very_long_header_2)
	eprintln(term.header(['abcd'].repeat(500).join(' '), '_-/\\'))
	eprintln(term.header(['abcd'].repeat(500).join(' '), '_-//'))
	eprintln(term.header('1', '_-/\\\/'))
	eprintln(term.header('12', '_-/\\\/'))
	eprintln(term.header('123', '_-/\\\/'))
	eprintln(term.header('1234', '_-/\\/\\'))
	eprintln(term.header('', '-'))
    */
	assert term_width == empty_header.len
	assert term_width == short_header.len
	assert term_width == very_long_header.len
	assert term_width == very_long_header_2.len
	assert term_width == term.header('1234', '_-/\\/\\').len
}
