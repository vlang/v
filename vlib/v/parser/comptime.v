module parser

import (
	v.ast
)

pub fn (p mut Parser) comp_if() ast.CompIf {
	p.next()
	p.check(.key_if)
	if p.tok.kind == .not {
		p.next()
	}
	p.check_name()
	if p.tok.kind == .question {
		p.next()
	}
	p.parse_block()
	if p.tok.kind == .dollar && p.peek_tok.kind == .key_else {
		p.next()
		p.check(.key_else)
		p.parse_block()
	}
	return ast.CompIf{}
}
