module types

fn test_one_group_wraps_the_whole_expression() {
	assert text_is_a_single_parenthesised_group('(a + b)')
	assert text_is_a_single_parenthesised_group('((a + b))')
	assert text_is_a_single_parenthesised_group('(f(a), g(b))')
	assert !text_is_a_single_parenthesised_group('(a) + (b)')
	assert !text_is_a_single_parenthesised_group('(a) * m')
	assert !text_is_a_single_parenthesised_group('a + b')
	assert !text_is_a_single_parenthesised_group('(')
	assert !text_is_a_single_parenthesised_group('')
}

fn test_a_parenthesis_of_a_comment_or_a_literal_is_not_syntax() {
	assert text_is_a_single_parenthesised_group('(value /* ) */)')
	assert text_is_a_single_parenthesised_group('(value // )\n)')
	assert text_is_a_single_parenthesised_group("(value + ')'.len)")
	assert !text_is_a_single_parenthesised_group('(a /* ) */) + (b)')
}

fn test_a_comment_around_the_group_is_not_part_of_it() {
	assert text_is_a_single_parenthesised_group('(input) /* explanation */')
	assert text_is_a_single_parenthesised_group('/* leading */ (input)')
	assert text_is_a_single_parenthesised_group('(input) // trailing\n')
	assert !text_is_a_single_parenthesised_group('(a) /* c */ + (b)')
	assert !text_is_a_single_parenthesised_group('/* only a comment */')
}

fn test_a_literal_inside_an_interpolation_does_not_end_the_string() {
	// An interpolation holds code, which may quote the same way the string
	// around it does, so the parenthesis in it is still not syntax.
	assert text_is_a_single_parenthesised_group("('\${')'}')")
	assert text_is_a_single_parenthesised_group("(('\${')'}'))")
	assert !text_is_a_single_parenthesised_group("('\${')'}') + (b)")
	// Braces of a nested literal do not close the interpolation either.
	assert text_is_a_single_parenthesised_group("('\${f('}')}' + ')')")
	// A raw string interpolates nothing, so its `\${` opens no code.
	assert text_is_a_single_parenthesised_group("(r'\${' + ')')")
	assert !text_is_a_single_parenthesised_group("(r'\${') + (b)")
}
