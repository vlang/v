module parser

import v.ast

fn (mut p Parser) lock_expr() ast.LockExpr {
	pos := p.tok.position()
	is_rlock := p.tok.kind == .key_rlock
	p.next()
	lockeds := p.expr_list()
	stmts := p.parse_block()
	return ast.LockExpr {
		lockeds: lockeds
		stmts: stmts
		is_rlock: is_rlock
		pos: pos
	}
}

	
