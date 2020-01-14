module toml

import (
	os
	strings
)

const (
	err_syntax = ('toml: syntax error.')
)

struct Parser{
mut:
	scanner 	&Scanner
	data		TOML
	token_idx	int
	tok			TokenKind
	prev_tok	TokenKind
	tokens		[]Token
	lit			string
}

fn (t TOML) new_parser(scanner &Scanner) Parser {
	mut p := Parser{
		scanner: scanner
	}
	p.scan_tokens()
	return p
}

fn (p mut Parser) scan_tokens(){
	for {
		res := p.scanner.scan()
		p.tokens << Token {
			tok: res.tok
			lit: res.lit
			line_nr: p.scanner
			pos: p.scanner.pos
		}
		if res.tok == .eof{
			break
		}
	}
}

fn (p Parser) data() TOML {
	return p.data
}