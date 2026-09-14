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
