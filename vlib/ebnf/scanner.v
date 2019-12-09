module ebnf

const(
	err_sinchar_exp = error('ebnf: single char expected. found:$s')
	err_missing_pro = error('ebnf: missing production:')
	err_non_lex_pro = error('ebnf:')
	err_dec_char = error('')
	err_is_unreach = error('')
)

enum TokenKind(
	eof
	name
	expression
	equal
	vertical	// |
	lbrack		// [
	rbrack 		// ]
	lcurry		// {
	rcurry		// }
	lparen		// (
	rparen		// )
	tok_pro	 	
	tok_alt  
	tok_term
	tok_opt
	tok_group
	tok_repetition
)

struct Scanner{
	file_path string
	text string
mut:
	pos int
}

struct Parser{
	scanner &Scanner
mut:
	tok rune
	pos int
}