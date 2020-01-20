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
	name
	number
	str
	scalars
	comma
	question
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
	mut s := [''].repeat(nr_tokens)
	s[Kind.eof] = 'eof'
	s[Kind.name] = 'name'
	s[Kind.number] = 'number'
	s[Kind.str] = 'STR'
	s[Kind.scalars] = 'scalars'
	s[Kind.comma] = '.'
	s[Kind.question] = '?'
	s[Kind.hyphen] = '-'
}