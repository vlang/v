module parser

import (
	v.ast
)

pub fn (p mut Parser) comp_if() ast.CompIf {
	pos := p.tok.position()
	p.next()
	p.check(.key_if)
	is_not := p.tok.kind == .not
	if is_not {
		p.next()
	}
	val := p.check_name()
	if p.tok.kind == .question {
		p.next()
	}
	mut node := ast.CompIf{
		is_not: is_not
		stmts: p.parse_block()
		pos: pos
		val: val
	}
	if p.tok.kind == .dollar && p.peek_tok.kind == .key_else {
		p.next()
		p.check(.key_else)
		node.has_else = true
		node.else_stmts = p.parse_block()
	}
	return node
}
