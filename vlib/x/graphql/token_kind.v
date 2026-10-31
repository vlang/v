module graphql

const _token_kind_map = {
	TokenKind.sof:           '<SOF>'
	TokenKind.eof:           '<EOF>'
	TokenKind.bang:          '!'
	TokenKind.question_mark: '?'
	TokenKind.dollar:        '$'
	TokenKind.amp:           '&'
	TokenKind.paren_l:       '('
	TokenKind.paren_r:       ')'
	TokenKind.spread:        '...'
	TokenKind.colon:         ':'
	TokenKind.equals:        '='
	TokenKind.at:            '@'
	TokenKind.bracket_l:     '['
	TokenKind.bracket_r:     ']'
	TokenKind.brace_l:       '{'
	TokenKind.pipe:          '|'
	TokenKind.brace_r:       '}'
	TokenKind.name:          'Name'
	TokenKind.integer:       'Int'
	TokenKind.float:         'Float'
	TokenKind.string_value:  'String'
	TokenKind.block_string:  'BlockString'
	TokenKind.comment:       'Comment'
}

// An exported enum describing the different kinds of tokens that the
// lexer emits.
pub enum TokenKind {
	sof
	eof
	bang
	question_mark
	dollar
	amp
	paren_l
	paren_r
	spread
	colon
	equals
	at
	bracket_l
	bracket_r
	brace_l
	pipe
	brace_r
	name
	integer
	float
	string_value
	block_string
	comment
}

// Returns the character value of the TokenKind enum
pub fn (t TokenKind) gql_str() string {
	return graphql._token_kind_map[t]
}
