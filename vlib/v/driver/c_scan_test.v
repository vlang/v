module driver

fn test_comments_literals_and_numbers_of_c_are_passed_over() {
	text := 'a /* b */ c // d\ne "f\\"g" h \'i\' 0x1fUL j 1\'000 k'
	assert v3_c_skip_comment(text, 2) == 9
	assert v3_c_skip_comment(text, 12) == text.index('\n') or { -1 }
	assert v3_c_skip_comment(text, 0) == 0
	assert v3_c_skip_comment('/* open', 0) == 7
	quote := text.index('"') or { -1 }
	assert text[v3_c_skip_literal(text, quote)..].starts_with(' h')
	// A literal ends with its line at the latest.
	assert v3_c_skip_literal('"open\nnext', 0) == 5
	number := text.index('0x') or { -1 }
	assert text[v3_c_skip_number(text, number)..].starts_with(' j')
	separated := text.index("1'") or { -1 }
	assert text[v3_c_skip_number(text, separated)..].starts_with(' k')
	assert v3_c_identifier_end('name_1+2', 0) == 6
	assert v3_c_identifier_end('1name', 0) == 0
	assert v3_c_identifiers(text) == ['a', 'c', 'e', 'h', 'j', 'k']
}

fn test_identifiers_of_a_line_leave_out_numbers_and_literals() {
	assert v3_c_identifiers('defined(A) && B > 0x10UL || C == \'D\' || !strcmp("E", F)') == [
		'defined',
		'A',
		'B',
		'C',
		'strcmp',
		'F',
	]
}
