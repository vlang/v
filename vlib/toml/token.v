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
	comment			// #
	name			// ABC
	basic_str		// "ABC"
	lit_str			// 'ABC'
	array			
	table			
	array_of_table
	table_of_array	
	wquote			// "
	squote			// '
	number			// 123
	begin
	end
	nul
	dot
	comma
	equal
	lcbr			// {
	rcbr			// }
	creturn 		// Carriage Return
	newline			// Line Feed
	lsbr			// [
	rsbr			// ]
	hyphen
	coron
	b_header		// 0b
	h_header		// 0x
	o_header		// 0o
	bool_true		// true
	bool_false		// false
	table
	array
	integer
	binary
	double
	timestamp
}

fn build_keys() map[string]int {
	mut res := map[string]int
	for t := int(TokenKind.keyword_beg) + 1; t < int(TokenKind.keyword_end); t++ {
		key := TokenStr[t]
		res[key] = int(t)
	}
	return res
}

fn build_token_str() []string{
	mut s := [''].repeat(NrToken)
	s[TokenKind.eof] = 'eof'
	s[TokenKind.comment] = '#'
	s[TokenKind.wquote] = '"'
	s[TokenKind.squote] = '\''
	s[TokenKind.dot] = '.'
	s[TokenKind.comma] = ','
	s[TokenKind.lbcr] = '{'
	s[TokenKind.rbcr] = '}'
	s[TokenKind.creturn] = '\r'
	s[TokenKind.newline] = '\n'
	s[TokenKind.lsbr] = '['
	s[TokenKind.rsbr] = ']'
	s[TokenKind.hyphen] = '-'
	s[TokenKind.coron] = ':'
	s[TokenKind.b_header] = '0b'
	s[TokenKind.h_header] = '0x'
	s[TokenKind.o_header] = '0o'
	s[TokenKind.bool_true] = 'true'
	s[TokenKind.bool_false] = 'false'
}