import toml

sturct Token{
	token	 TokenKind
	lit		 string
	line_nr  int 
	name_idx int
	col		 int
}

enum TokenKind{
	eof
	comment
	quote
	form_feed
	doublequote
	nul
	dot
	comma
	equal
	lbrace
	rbrace
	creturn
	newline
	lbrackert
	rbrackert
	hyphen
	coron
	bin_header
	hex_header
	oct_header
	bool_true
	bool_false
	integer
	binary
	double
	timestamp
	wquote_string
	quote_string
	wquote_multi_line
	quote_multi_line
}

fn build_token_str() []string{
	mut s := [''].repeat(NrToken)
	s[TokenKind.eof] = 'eof'
	
}