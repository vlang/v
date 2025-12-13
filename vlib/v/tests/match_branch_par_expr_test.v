module main

enum TokenType {
	null
	word
	get_word
	set_word
	lit_word
	int_value
	dec_value
	bin_value
	str_value
	block_start
	block_end
	expr_start
	expr_end
	comment
}

fn test_match_branch_par_expr() {
	t := TokenType.word
	s := match t {
		(TokenType.null) { '' }
		(TokenType.word) { 'a' }
		(TokenType.get_word) { 'b' }
		(TokenType.set_word) { 'c' }
		(TokenType.lit_word) { 'd' }
		else { 'other' }
	}

	assert s == 'a'
}
