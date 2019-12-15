module yaml

struct Token{
	tok 		TokenKind
	lit			string
	line_nr 	int
	name_idx 	int
	pos 		int
}

enum TokenKind{
	eof
	comma
	hyphen
	rangle
	vertcal
	hash
	seprate
	space
	coron
	json
	name
	tab
}

fn build_token_str() []string{

}