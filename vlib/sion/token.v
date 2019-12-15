module sion

struct Token{
	token	 TokenKind
	lit		 string
	line_nr  int 
	name_idx int
	pos		 int
}

enum TokenKind{
	eof
	nil
	date
	data
	integer
	double
	str
}