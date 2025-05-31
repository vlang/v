import os
import term
import strings

fn test_get_terminal_size() {
	cols, _ := term.get_terminal_size()
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

fn test_get_cursor_position() {
	original_position := term.get_cursor_position()!
	cursor_position_1 := term.get_cursor_position()!
	assert original_position.x == cursor_position_1.x
	assert original_position.y == cursor_position_1.y

	term.set_cursor_position(
		x: 10
		y: 11
	)
	cursor_position_2 := term.get_cursor_position()!

	term.set_cursor_position(
		x: 5
		y: 6
	)
	cursor_position_3 := term.get_cursor_position()!

	term.set_cursor_position(original_position)
	eprintln('original_position: ${original_position}')
	eprintln('cursor_position_2: ${cursor_position_2}')
	eprintln('cursor_position_3: ${cursor_position_3}')
	// 0,0 is returned on dumb terminals
	if cursor_position_2.x == 0 && cursor_position_2.y == 0 {
		return
	}
	if cursor_position_3.x == 0 && cursor_position_3.y == 0 {
		return
	}
	assert cursor_position_2.x == 10
	assert cursor_position_2.y == 11
	assert cursor_position_3.x == 5
	assert cursor_position_3.y == 6
}

fn test_set_terminal_title() {
	// do not change the current terminal title outside of CI:
	if os.getenv('CI') != 'true' {
		return
	}
	term.set_terminal_title('v is awesome!')
	dump(@FILE)
	assert true
}

fn test_set_tab_title() {
	// do not change the current terminal tab title outside of CI:
	if os.getenv('CI') != 'true' {
		return
	}
	term.set_tab_title('v is awesome!')
	dump(@FILE)
	assert true
}

fn test_strip_ansi() {
	string_list := [
		'abc',
		term.bold('abc'),
		term.yellow('abc'),
		term.bold(term.red('abc')),
		term.strikethrough(term.inverse(term.dim(term.bold(term.bright_bg_blue('abc'))))),
	]
	for s in string_list {
		assert term.strip_ansi(s) == 'abc'
	}
}

fn test_write_color() {
	mut sb := strings.new_builder(100)
	term.writeln_color(mut sb, 'hello')
	term.writeln_color(mut sb, 'hello', fg: .red)
	term.writeln_color(mut sb, 'hello', bg: .cyan)
	term.writeln_color(mut sb, 'hello', styles: [.bold, .italic, .underline], fg: .red, bg: .cyan)
	term.writeln_color(mut sb, 'hello', custom: '38;5;214')

	output := sb.str()
	println(output)

	assert output.contains('\x1b[0m')
	assert output.contains('\x1b[31m')
	assert output.contains('\x1b[46m')
	assert output.contains('\x1b[1;3;4;31;46m')
	assert output.contains('\x1b[38;5;214m')
}
