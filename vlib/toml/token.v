module toml

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
	form_feed
	nul
	dot
	comma
	equal
	lcbr			//{
	rcbr			//}
	creturn 		//Carriage Return
	newline			//Line Feed
	lsbr			//[
	rsbr			//]
	hyphen
	coron
	b_header
	h_header
	o_header
	bool_true
	bool_false
	integer
	binary
	double
	timestamp
	char
	multi_char
}

fn build_keys() map[string]int{
	mut res := map[string]int

}

fn build_token_str() []string{
	mut s := [''].repeat(NrToken)
	s[TokenKind.eof] = 'eof'
	s[TokenKind.comment] = '#'
}