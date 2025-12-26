module parser

import v.ast

fn (mut p Parser) lock_expr() ast.LockExpr {
	// TODO: Handle aliasing sync
	p.register_auto_import('sync')
	p.open_scope()
	defer {
		p.close_scope()
	}
	mut pos := p.tok.pos()
	mut lockeds := []ast.Expr{}
	mut is_rlocked := []bool{}
	for {
		is_rlock := p.tok.kind == .key_rlock
		if !is_rlock && p.tok.kind != .key_lock {
			p.unexpected(expecting: 'one or more shared variable names')
		}
		p.next()
		if p.tok.kind == .lcbr {
			break
		}
		if p.tok.kind == .name {
			exprs := p.expr_list(true)
			for e in exprs {
				if !e.is_lockable() {
					p.error_with_pos('`${e}` cannot be locked - only `x`, `x.y` or `x.$(y)` are supported',
						e.pos())
				}
				lockeds << e
				is_rlocked << is_rlock
			}
		}
		if p.tok.kind == .lcbr {
			break
		}
		if p.tok.kind == .semicolon {
			p.next()
		}
	}
	stmts := p.parse_block_no_scope(false)
	scope := p.scope
	pos.update_last_line(p.prev_tok.line_nr)
	return ast.LockExpr{
		lockeds:  lockeds
		stmts:    stmts
		is_rlock: is_rlocked
		pos:      pos
		scope:    scope
	}
}
